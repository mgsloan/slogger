{-# LANGUAGE TemplateHaskell #-}

module Types where

import           Control.Monad.Logger
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as B8
import qualified Data.Text as T
import           Language.Haskell.TH (Name)
import           Language.Haskell.TH.Lift
import           System.Log.FastLogger

--FIXME: having 'getLastInfo' could be expensive when it comes to
--freeing up memory...

class MonadLogger m => MonadSlogger m where
    getLastInfo :: m (Maybe (LogId, LogInfo))
    setLastInfo :: Maybe (LogId, LogInfo) -> m ()
    getNextId :: m LogId
    getIdParents :: m [LogId]
    setIdParents :: [LogId] -> m ()

type LogTag = T.Text

data LogChunk
    = LogLevel LogLevel
    | LogTags [LogTag]
    | LogChunk LogStr
    | LogLoc Loc
    | LogSource LogSource
    | LogRef Name
    | LogDataOffset LogDataOffset
    | LogEmpty

instance Lift LogChunk where
    lift (LogLevel x) = [| LogLevel x |]
    lift (LogTags x) = [| LogTags (map T.pack txts) |]
      where
        txts = map T.unpack x
    lift (LogChunk x) = [| LogChunk x |]
    lift (LogLoc x) = [| LogLoc $(liftLoc x) |]
    lift (LogSource x) = [| LogSource (T.pack txt) |]
      where
        txt = T.unpack x
    lift (LogRef x) = [| LogRef x |]
    lift (LogDataOffset x) = [| LogDataOffset x |]
    lift LogEmpty = [| LogEmpty |]

data LogInfo = LogInfo
    { infoLoc :: Loc
    , infoSource :: LogSource
    , infoLevel :: LogLevel
    , infoTags :: [LogTag]
    , infoDataOffset :: Maybe LogDataOffset
    , infoStr :: LogStr
    }

data LogData = LogData
    { logId :: LogId
    , logParentId :: Maybe LogId
    , dataOffset :: LogDataOffset
    }

type LogId = Int
type LogDataOffset = Integer

-- Orphan instances

instance Lift LogStr where
    lift x = [| toLogStr (B8.pack bs :: B.ByteString) |]
      where
        bs = B8.unpack (fromLogStr x)
