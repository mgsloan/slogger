{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE ParallelListComp #-}

module LogFunc
    ( LogFuncSettings (..)
    , LogFunc (..)
    , ToLogChunk (..)
    ) where

import           Control.Monad.Logger
import           Data.Binary
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Lazy as LB
import           Data.Maybe
import           Data.Monoid
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.Lazy as LT
import           Data.Traversable (forM)
import           Data.Typeable
import           Language.Haskell.TH
import           Language.Haskell.TH.Lift (deriveLift)
import           Language.Haskell.TH.Syntax
import           Prelude hiding (log)
import           Safe (headMay)
import           System.Log.FastLogger
import           TypedData
import           Types

--TODO:
--
-- * Output the tags
--
-- * Switch to a Monoid on LogInfo??

data LogFuncSettings = LogFuncSettings

$(deriveLift ''LogFuncSettings)

class LogFunc a where
    logFunc :: LogFuncSettings -> [LogChunk] -> a
    default logFunc :: (MonadSlogger m, a ~ m ()) => LogFuncSettings -> [LogChunk] -> a
    logFunc = defaultLogFunc

defaultLogFunc :: (MonadSlogger m, a ~ m ()) => LogFuncSettings -> [LogChunk] -> a
defaultLogFunc lfs xs = do
    let info@(LogInfo loc source level tags dat str) = renderLogInfo lfs xs
    lid <- getNextId
    parents <- getIdParents
    moffset <- persistData dat
    let metaDataStr = renderMetaData $ LogMetaData lid (headMay parents) moffset
    monadLoggerLog loc source level (str <> metaDataStr)
    setLastInfo (Just (lid, info))

renderMetaData :: LogMetaData -> LogStr
renderMetaData lmd =
    " [slog id=" <> toLogStr (show (logId lmd)) <>
    " pid=" <> toLogStr (show (fromMaybe (-1) (logParentId lmd))) <>
    " offset=" <> toLogStr (show (fromMaybe (-1) (logDataOffset lmd))) <>
    "]"

instance (ToLogChunk a, LogFunc b) => LogFunc (a -> b) where
    logFunc lfs xs x = logFunc lfs (toLogChunk x : xs)

instance (a ~ Exp) => LogFunc (Q a) where
    logFunc lfs xs = do
        loc <- location
        --TODO: filtering
        appsE $ varE 'logFunc : lift lfs : listE [] : (Prelude.map lift (xs) ++ [liftLoc loc])

--TODO: Consider moving this to a special, optional module intended to
--be imported by applications?

-- instance (a ~ ()) => LogFunc (IO a) where
--     logFunc lfs xs = B8.putStrLn (renderByteString lfs xs)

-- renderText :: LogFuncSettings -> [LogChunk] -> T.Text
-- renderText lfs = T.decodeUtf8 . fromLogStr . renderLogStr lfs

-- renderByteString :: LogFuncSettings -> [LogChunk] -> B8.ByteString
-- renderByteString lfs = fromLogStr . renderLogStr lfs

-- renderLogStr :: LogFuncSettings -> [LogChunk] -> LogStr
-- renderLogStr lfs xs = defaultLogStr loc source level str
--   where
--     LogInfo loc source level tags dat str = renderLogInfo lfs xs

renderLogInfo :: LogFuncSettings -> [LogChunk] -> LogInfo
renderLogInfo _ (Prelude.reverse -> xs) = LogInfo
    (fromMaybe defaultLoc $ headMay [l | LogLoc l <- xs])
    (fromMaybe ""         $ headMay [x | LogSource x <- xs])
    (fromMaybe LevelInfo  $ headMay [x | LogLevel x <- xs])
    (concat [x | LogTags x <- xs])
    (LogData [(sp, dat) | sp <- spans | (Just dat, _) <- chunks])
    (mconcat (map snd chunks))
  where
    chunks = [(x, y) | LogChunk x y <- xs]
    spans = zipWith LogSpan offsets (tail offsets)
    offsets = scanl (\acc (_, ll) -> acc + logStrLength ll) 0 chunks

-- TODO: have the default truncate the interpolant

class ToLogChunk a where
    toLogChunk :: a -> LogChunk
    default toLogChunk :: (Show a, Binary a, Typeable a) => a -> LogChunk
    toLogChunk = defaultToLogChunk

defaultToLogChunk :: (Show a, Binary a, Typeable a) => a -> LogChunk
defaultToLogChunk x = LogChunk (Just (toRawData x)) (toLogStr (show x))

instance ToLogChunk LogChunk where toLogChunk = id
instance ToLogChunk LogLevel where toLogChunk = LogLevel
instance ToLogChunk Loc      where toLogChunk = LogLoc
instance ToLogChunk Name     where toLogChunk = LogRef

-- Use the default implementation of toLogChunk to define instances
-- for all ToLogChunk instances.

-- Specialness of String and LogStr: not escaped
--
-- TODO: make sure this works well with IsString defaulting.
instance ToLogChunk String where toLogChunk = LogChunk Nothing . toLogStr
instance ToLogChunk LogStr where toLogChunk = LogChunk Nothing

-- FIXME
-- instance ToLogChunk T.Text
-- instance ToLogChunk LT.Text

instance ToLogChunk B.ByteString
instance ToLogChunk LB.ByteString
instance ToLogChunk Int

{-
data Shown a = (Show a, Binary a) => Shown a

data OnlyShown a = Show a => OnlyShown a

data Store a = Binary a => Store LogStr a
-}

-- Same as defaultLoc in Control.Monad.Logger
defaultLoc :: Loc
defaultLoc = Loc "<unknown>" "<unknown>" "<unknown>" (0,0) (0,0)
