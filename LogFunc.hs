{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module LogFunc
    ( LogFuncSettings (..)
    , LogFunc (..)
    , ToLogChunk (..)
    ) where

import           Control.Monad.Logger
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Lazy as LB
import           Data.Maybe
import           Data.Monoid
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.Lazy as LT
import           Language.Haskell.TH
import           Language.Haskell.TH.Lift (deriveLift)
import           Language.Haskell.TH.Syntax
import           Prelude hiding (log)
import           Safe (headMay)
import           System.Log.FastLogger
import           Types

--TODO:
-- * Output the tags
--
-- * Switch to a Monoid on LogInfo??

data LogFuncSettings = LogFuncSettings

$(deriveLift ''LogFuncSettings)

class LogFunc a where
    logFunc :: LogFuncSettings -> [LogChunk] -> a
    default logFunc :: (MonadSlogger m, a ~ m ()) => LogFuncSettings -> [LogChunk] -> a
    logFunc lfs xs = do
        let info@(LogInfo loc source level tags mdat str) = logFunc lfs xs
        lid <- getNextId
        parents <- getIdParents
        monadLoggerLog loc source level str
        setLastInfo (Just (lid, info))

--TODO: turn into a function
instance LogFunc LogInfo where
    logFunc _ (Prelude.reverse -> xs) = LogInfo
        (fromMaybe defaultLoc $ headMay [l | LogLoc l <- xs])
        (fromMaybe ""         $ headMay [x | LogSource x <- xs])
        (fromMaybe LevelInfo  $ headMay [x | LogLevel x <- xs])
        (concat [x | LogTags x <- xs])
        (headMay [x | LogDataOffset x <- xs])
        (mconcat [x | LogChunk x <- xs])

--TODO: turn into a function
instance LogFunc LogStr where
    logFunc lfs xs = defaultLogStr loc source level str
      where
        LogInfo loc source level tags mdat str = logFunc lfs xs

--TODO: turn into a function
instance LogFunc B.ByteString where
    logFunc lfs = fromLogStr . logFunc lfs

--TODO: turn into a function
instance LogFunc T.Text where
    logFunc lfs = T.decodeUtf8 . fromLogStr . logFunc lfs

instance (a ~ Exp) => LogFunc (Q a) where
    logFunc lfs xs = do
        loc <- location
        --TODO: filtering
        appsE $ varE 'logFunc : lift lfs : listE [] : (Prelude.map lift (xs) ++ [liftLoc loc])

--TODO: Consider moving this to a special, optional module intended to
--be imported by applications?
instance (a ~ ()) => LogFunc (IO a) where
    logFunc lfs xs = B8.putStrLn (logFunc lfs xs)

instance (ToLogChunk a, LogFunc b) => LogFunc (a -> b) where
    logFunc lfs xs x = logFunc lfs (toLogChunk x : xs)

class ToLogChunk a where
    toLogChunk :: a -> LogChunk
    default toLogChunk :: ToLogStr a => a -> LogChunk
    toLogChunk = LogChunk . toLogStr

instance ToLogChunk LogChunk where toLogChunk = id
instance ToLogChunk LogLevel where toLogChunk = LogLevel
instance ToLogChunk Loc      where toLogChunk = LogLoc
instance ToLogChunk Name     where toLogChunk = LogRef

-- Use the default implementation of toLogChunk to define instances
-- for all ToLogChunk instances.

instance ToLogChunk String
instance ToLogChunk T.Text
instance ToLogChunk LT.Text
instance ToLogChunk LogStr
instance ToLogChunk B.ByteString
instance ToLogChunk LB.ByteString

-- Same as defaultLoc in Control.Monad.Logger
defaultLoc :: Loc
defaultLoc = Loc "<unknown>" "<unknown>" "<unknown>" (0,0) (0,0)
