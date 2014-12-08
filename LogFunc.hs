{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ParallelListComp #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}

module LogFunc where

import           Control.Monad.Logger
import           Data.Binary
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as LB
import           Data.Maybe
import           Data.Monoid
import           Data.Typeable
import           Language.Haskell.TH
import           Language.Haskell.TH.Syntax
import           Prelude hiding (log)
import           Safe (headMay)
import           System.Log.FastLogger
import           TypedData
import           Types

class LogFunc a where
    logFunc :: [LogChunk] -> a
    default logFunc :: (MonadSlogger m, a ~ m ()) => [LogChunk] -> a
    logFunc = defaultLogFunc

instance (ToLogChunk a, LogFunc b) => LogFunc (a -> b) where
    logFunc xs x = logFunc (toLogChunk x : xs)

instance (a ~ Exp) => LogFunc (Q a) where
    logFunc xs = do
        loc <- location
        appsE $ varE 'logFunc : listE [] : (Prelude.map lift (xs) ++ [liftLoc loc])

class ToLogChunk a where
    toLogChunk :: a -> LogChunk
    default toLogChunk :: (Show a, Binary a, Typeable a) => a -> LogChunk
    toLogChunk = defaultToLogChunk

instance ToLogChunk LogChunk where toLogChunk = id
instance ToLogChunk LogLevel where toLogChunk = LogLevel
instance ToLogChunk Loc      where toLogChunk = LogLoc
instance ToLogChunk Name     where toLogChunk = LogRef

-- Specialness of String and LogStr: not escaped
--
-- TODO: make sure this works well with IsString defaulting.
instance ToLogChunk String where toLogChunk = LogChunk Nothing . toLogStr
instance ToLogChunk LogStr where toLogChunk = LogChunk Nothing

-- Use the default implementation of toLogChunk to define instances
-- for all ToLogChunk instances.

instance ToLogChunk B.ByteString
instance ToLogChunk LB.ByteString
instance ToLogChunk Int

-- Implementation of defaults

-- TODO: truncate the shown version?
defaultToLogChunk :: (Show a, Binary a, Typeable a) => a -> LogChunk
defaultToLogChunk x = LogChunk (Just (toRawData x)) (toLogStr (show x))

defaultLogFunc :: (MonadSlogger m, a ~ m ()) => [LogChunk] -> a
defaultLogFunc xs = do
    _ <- sloggerLog (logChunksToInfo xs)
    return ()

logChunksToInfo :: [LogChunk] -> LogInfo
logChunksToInfo (Prelude.reverse -> xs) = LogInfo
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

-- Same as defaultLoc in Control.Monad.Logger
defaultLoc :: Loc
defaultLoc = Loc "<unknown>" "<unknown>" "<unknown>" (0,0) (0,0)
