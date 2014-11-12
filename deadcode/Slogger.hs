{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE ViewPatterns #-}

module Slogger where

import           Control.Applicative
import           Control.Arrow (first, second)
import           Control.Concurrent.Async
import           Control.Concurrent.Lifted (fork)
import           Control.Concurrent.MVar.Lifted
import           Control.Monad.Logger
import           Control.Monad.State
import           Data.Binary (Binary, encode, decode)
import qualified Data.Binary.Typed.Internal as BT
import qualified Data.ByteString.Lazy as LB
import           Data.Conduit
import           Data.Conduit.Binary (sourceHandle, sinkHandle)
import qualified Data.Conduit.List as CL
import           Data.Conduit.Serialization.Binary (conduitDecode, conduitEncode)
import           Data.IORef
import           Data.List (intersperse, find)
import           Data.Maybe
import           Data.Monoid
import           Data.String
import qualified Data.Text as T
import           Data.Text.Encoding (encodeUtf8)
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Builder as TB
import           Data.Typeable
import           GHC.Generics
import           GHC.IO.Handle
import qualified Language.Haskell.TH.Syntax as TH
import           Prelude hiding (log)
import           Safe (headMay)
import           System.IO
import           System.Log.FastLogger (ToLogStr(..), LogStr)
import           System.Process


newtype SloggerT m a = SloggerT (StateT SloggerState m a)
    deriving (Functor, Applicative, Monad, MonadFix, MonadPlus, Alternative, MonadTrans, MonadIO, MonadLogger)

-- deriving instance MonadLogger m => MonadLogger (SloggerT m)

data SloggerState = SloggerState
    { nextIdRef :: IORef LogId
    , idParents :: [LogId]
    }

evalSloggerT :: MonadIO m => SloggerT m a -> m a
evalSloggerT (SloggerT m) = do
    nextIdRef <- liftIO $ newIORef 1
    let idParents = []
    evalStateT m SloggerState {..}

-- TODO: Replace this business with lenses ??

getSloggerState :: Monad m => SloggerT m SloggerState
getSloggerState = SloggerT get

putSloggerState :: Monad m => SloggerState -> SloggerT m ()
putSloggerState = SloggerT . put

modifySloggerState :: Monad m => (SloggerState -> SloggerState) -> SloggerT m ()
modifySloggerState f = do
    ss <- getSloggerState
    putSloggerState (f ss)

modifyParents :: Monad m => ([LogId] -> [LogId]) -> SloggerT m ()
modifyParents f = modifySloggerState (\ss -> ss { idParents = f (idParents ss) })

-- Log functions

logTH :: TH.Name -> LogLevel -> TH.Q TH.Exp
logTH func level =
    [|\x -> $(return (TH.VarE func)) $(TH.qLocation >>= liftLoc) (T.pack "") $(TH.lift level) (x :: T.Text)|]

logDebug, logInfo, logWarn, logError :: TH.Q TH.Exp
logDebug = logTH 'log LevelDebug
logInfo = logTH 'log LevelInfo
logWarn = logTH 'log LevelWarn
logError = logTH 'log LevelError

logNestDebug, logNestInfo, logNestWarn, logNestError :: TH.Q TH.Exp
logNestDebug = logTH 'logNest LevelDebug
logNestInfo = logTH 'logNest LevelInfo
logNestWarn = logTH 'logNest LevelWarn
logNestError = logTH 'logNest LevelError

log :: (MonadLogger m, MonadIO m, Functor m, ToLogStr msg) => Loc -> LogSource -> LogLevel -> msg -> SloggerT m ()
log loc source level msg = void $ logInternal loc source level Nothing msg "slog"

logStart :: (MonadLogger m, MonadIO m, ToLogStr msg) => Loc -> LogSource -> LogLevel -> msg -> SloggerT m LogId
logStart loc source level msg = do
    lid <- logInternal loc source level Nothing msg "slogStart"
    modifyParents (lid:)
    return lid

logEnd :: (MonadLogger m, MonadIO m, Functor m, ToLogStr msg) => LogId -> Loc -> LogSource -> LogLevel -> msg -> SloggerT m ()
logEnd i loc source level msg = void $ logInternal loc source level (Just i) msg "slogEnd"

--TODO: exception handling
logNest :: (MonadLogger m, MonadIO m, Functor m, ToLogStr msg) => Loc -> LogSource -> LogLevel -> msg -> SloggerT m a -> SloggerT m a
logNest loc source level msg f = do
    lid <- logStart loc source level msg
    x <- f
    logEnd  lid loc source level msg
    return x

logFork loc source level msg f = do
    pidVar <- newEmptyMVar
    pid <- fork $ logNest loc source level (msg) f
    putMVar pidVar pid

logInternal :: (MonadLogger m, MonadIO m, ToLogStr msg) => Loc -> LogSource -> LogLevel -> Maybe LogId -> msg -> T.Text -> SloggerT m LogId
logInternal loc source level mid msg typ = do
    i <- maybe getFreshId return mid
    ss <- getSloggerState
    logInternal' loc source level msg typ (LogData i (headMay (idParents ss)) 0)
    return i

logInternal' :: (MonadLogger m, ToLogStr msg) => Loc -> LogSource -> LogLevel -> msg -> T.Text -> LogData -> m ()
logInternal' loc source level msg typ info = do
    let msg' = toLogStr msg <> " " <> serializeSD typ
            [ ("id", tshow (logId info))
            , ("pid", tshow (fromMaybe 0 (logParentId info)))
            , ("offset", tshow (dataOffset info))
            ]
    monadLoggerLog loc source level msg'

-- | Structured data in syslog format.
--
--   See https://tools.ietf.org/html/rfc5424#page-15
--
--   NOTE: Doesn't do anything to sanitize the "SD-ID", the first
--   parameter, or the "PARAM-NAME", the fst of the tuples.
serializeSD :: T.Text -> [(T.Text, T.Text)] -> LogStr
serializeSD sdId params =
    "[" <>
    toLogStr sdId <>
    mconcat (intersperse " " (map paramLogStr params)) <>
    "]"
  where
    paramLogStr (name, value) =
        toLogStr name <> "=" <> toLogStr (escapeValue value)
    escapeValue = T.replace "\\" "\\\\"
                . T.replace "\"" "\\\""
                . T.replace "]" "\\]"

findAndParseSD :: T.Text -> T.Text -> Either T.Text [(T.Text, T.Text)]
findAndParseSD sdId logStr = do
    found <-
        case findSD sdId logStr of
            Nothing -> Left $ "Couldn't find structured data " <> sdId
            Just found -> Right found
    parseSDParams found

findSD :: T.Text -> T.Text -> Maybe T.Text
findSD sdId logStr =
    case T.breakOnAll needle logStr of
        [(_, extractParams . T.drop (T.length needle) -> Just params)] ->
            Just params
        _ -> Nothing
  where
    needle = "[" <> sdId <> " "
    extractParams = fmap fst . extractNonEscaped ']'

extractNonEscaped :: Char -> T.Text -> Maybe (T.Text, T.Text)
extractNonEscaped c =
    find (not . T.isSuffixOf "\\" . fst) .
    T.breakOnAll (T.singleton c)

--FIXME: doesn't seem to work yet.

parseSDParams :: T.Text -> Either T.Text [(T.Text, T.Text)]
parseSDParams x | T.null x = Right []
parseSDParams
    (T.break (== '=') -> (name, T.stripPrefix "=\"" -> Just value)) = do
        (value, postfix) <- parseSDValue value
        xs <- parseSDParams postfix
        return ((name, value) : xs)
parseSDParams _ = Left "Invalid key-value pair in syslog structured data."

-- Note: assumes that we've already parsed in the first quote.
parseSDValue :: T.Text -> Either T.Text (T.Text, T.Text)
parseSDValue = fmap (first (TL.toStrict . TB.toLazyText)) . helper 0
  where
    helper n (T.uncons -> Just (ch, txt')) =
        case ch of
            '\\' ->
                case T.uncons txt' of
                    Just ('"', txt'') -> fmap (first (TB.singleton '"' <>)) (helper (n+2) txt'')
                    Just ('\\', txt'') -> fmap (first (TB.singleton '\\' <>)) (helper (n+2) txt'')
                    Just (']', txt'') -> fmap (first (TB.singleton ']' <>)) (helper (n+2) txt'')
                    Just (c, txt'') -> fmap (first ((TB.singleton '\\' <>) . (TB.singleton c <>))) (helper (n+2) txt'')
                    Nothing -> Left ""
            '"' -> Right (mempty, txt')
            _ -> fmap (first (TB.singleton ch <>)) (helper (n+1) txt')
    helper n _ = Left $ "Missing ending quote in syslog structured data value: " <> tshow n

-- parseStructuredDataParams :: T.Text -> (Maybe Text, [(T.Text, T.Text)])
-- parseStructuredDataParams txt =
--     case mvalue of
--         Just (value, ) -> (Nothing, )
--         Nothing -> ()
--   where
--     (name, rest) = T.break "=" txt
--     mvalue =
--         extractNonEscaped '"' $
--         fromMaybe rest $
--         T.stripPrefix "\"" rest

    -- unescapeValue escaped
    --     | T.null after = before
    --     | T.null sansSlash = before <> "\\"
    --     | otherwise = before <> replacement <> T.tail sansSlash
    --   where
    --     (before, after) = T.break (== '\\') escaped
    --     sansSlash = T.drop 1 after
    --     replacement =
    --         case T.head sansSlash of
    --             '"' -> "\""
    --             '\\' -> "\\"
    --             ']' -> "]"
    --             c -> "\\" <> T.singleton c

-- Binary bits

-- TODO: memoize type serialization?  Serialize to a separate file?

appendTypedData :: (Typeable a, Binary a) => FilePath -> a -> IO Integer
appendTypedData fp x = withBinaryFile fp AppendMode (\h -> appendTypedData' h x)

readTypedData :: (Typeable a, Binary a) => FilePath -> Integer -> IO a
readTypedData fp pos = withBinaryFile fp ReadMode (\h -> readTypedData' h pos)

appendTypedData' :: forall a. (Typeable a, Binary a) => Handle -> a -> IO Integer
appendTypedData' h x = do
    let ty = BT.stripTypeRep (typeOf x)
    appendData h (RawData ty (encode x))

readTypedData' :: forall a. (Typeable a, Binary a) => Handle -> Integer -> IO a
readTypedData' h pos = do
    RawData ty bs <- readData h pos
    let expected = BT.stripTypeRep (typeOf (undefined :: a))
    if ty /= expected
        then fail $ "Type mismatch in decodeData (got " ++ show ty ++ ", but expected " ++ show expected ++ ")"
        else return $ decode bs

data RawData = RawData BT.TypeRep LB.ByteString
    deriving (Generic)

instance Binary RawData where

appendData :: Binary a => Handle -> a -> IO Integer
appendData h raw = do
    pos <- hFileSize h
    yield raw $= conduitEncode $$ sinkHandle h
    return pos

--TODO: handle EOF case

readData :: Binary a => Handle -> Integer -> IO a
readData h pos = do
    hSeek h AbsoluteSeek pos
    mdecoded <- sourceHandle h $= conduitDecode $$ CL.head
    maybe (fail "Failed to decode data.") return mdecoded

--TODO: avoid overhead of decoding twice, by computing the offset of
--the data which comes after the type.

ghciStr :: FilePath -> Integer -> IO String
ghciStr fp pos = do
    RawData ty _ <- withBinaryFile fp ReadMode (\h -> readData h pos)
    return $ "readTypedData \"" ++ fp ++ "\" " ++ show pos ++ " :: IO (" ++ show (BT.unStripTypeRep ty) ++ ")"

runGhciWithData :: FilePath -> Integer -> IO ()
runGhciWithData fp pos = do
    cmd <- ghciStr fp pos
    (hin, hout, herr, ph) <- runInteractiveProcess "ghci" ["Slogger.hs"] Nothing Nothing
    (sourceHandle hout $$ sinkHandle stdout) `concurrently`
        (sourceHandle herr $$ sinkHandle stderr) `concurrently` do
            yield (encodeUtf8 (T.pack ("x <- " ++ cmd ++ "\n"))) $$ sinkHandle hin
            (sourceHandle stdin $$ sinkHandle hin)
    return ()

tshow :: Show a => a -> T.Text
tshow = T.pack . show
