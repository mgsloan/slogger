{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Slog
    ( slog
    , snest
    , sfork
    , sfork'
    , runSloggerT
    , SloggerT
    -- re-exports
    , LogLevel (..)
    , LogChunk (..)
    , MonadSlogger (..)
    , LogFunc (..)
    , ToLogChunk (..)
    , LogTag
    , LogId
    , LogDataOffset
    ) where

import Control.Applicative
import Control.Concurrent.Lifted (fork)
import Control.Concurrent.MVar.Lifted
import Control.Monad.Base
import Control.Monad.Catch
import Control.Monad.IO.Class
import Control.Monad.Logger
import Control.Monad.State
import Control.Monad.Trans.Control
import Data.IORef
import Data.Maybe
import Data.Monoid
import Language.Haskell.TH
import LogFunc
import Safe (headMay)
import System.Log.FastLogger
import TypedData (appendData)
import Types

-- TODO: remove logJustTH, and use the instance constraint trick to
-- handle returning ()

slog :: LogFunc a => a
slog = logFunc LogFuncSettings []

snest :: MonadSlogger m => m a -> m a
snest = sloggerNest

sfork :: Q Exp
sfork = do
    loc <- location
    [| sforkInternal (Just $(liftLoc loc)) |]

sfork' :: (MonadSlogger m, LogFunc (m ()), MonadMask m, MonadBaseControl IO m) => m () -> m ()
sfork' = sforkInternal Nothing

sforkInternal :: forall m. (MonadSlogger m, LogFunc (m ()), MonadMask m, MonadBaseControl IO m) => Maybe Loc -> m () -> m ()
sforkInternal mloc f = do
    pidVar <- newEmptyMVar
    pid <- fork $ do
        pid <- takeMVar pidVar
        slog ("Forked thread " :: String) (show pid) (maybe LogEmpty toLogChunk mloc) :: m ()
        snest f
    putMVar pidVar pid

runSloggerT :: MonadIO m => SloggerT m a -> m a
runSloggerT (SloggerT m) = do
    lastInfoRef <- liftIO $ newIORef Nothing
    nextIdRef <- liftIO $ newIORef 1
    let idParents = []
    evalStateT m SloggerState {..}

newtype SloggerT m a = SloggerT { unSloggerT :: StateT SloggerState m a }
    deriving (Functor, Applicative, Monad, MonadFix, MonadPlus, Alternative, MonadTrans, MonadIO, MonadLogger, MonadMask, MonadThrow, MonadCatch)

instance MonadBase b m => MonadBase b (SloggerT m) where
    liftBase = liftBaseDefault

instance MonadTransControl SloggerT where
    newtype StT SloggerT a = StSloggerT {unStSloggerT :: StT (StateT SloggerState) a}
    liftWith = defaultLiftWith SloggerT unSloggerT StSloggerT
    restoreT = defaultRestoreT SloggerT unStSloggerT

instance MonadBaseControl b m => MonadBaseControl b (SloggerT m) where
    newtype StM (SloggerT m) a = StMSloggerT {unStMSloggerT :: ComposeSt SloggerT m a}
    liftBaseWith = defaultLiftBaseWith StMSloggerT
    restoreM = defaultRestoreM unStMSloggerT

instance (MonadLogger m, MonadIO m, MonadMask m, a ~ ()) => LogFunc (SloggerT m a)

instance (MonadLogger m, MonadIO m, MonadMask m) => MonadSlogger (SloggerT m) where
    sloggerLog info@(LogInfo loc source level tags dat str) = do
        lid <- getNextId
        parents <- getIdParents
        moffset <- persistData dat
        let metaDataStr = renderMetaData $ LogMetaData lid (headMay parents) moffset
        monadLoggerLog loc source level (str <> metaDataStr)
        setLastInfo (Just (lid, info))
    sloggerNest f = do
        minfo <- getLastInfo
        case minfo of
            Nothing -> f
            Just (lid, info) -> do
                parents <- getIdParents
                setIdParents (lid : parents)
                finally f $ do
                    () <- slog ("END " <> infoStr info)
                        (infoLoc info)
                        (LogSource (infoSource info))
                        (LogTags (infoTags info))
                    setIdParents parents

data SloggerState = SloggerState
    { nextIdRef :: IORef LogId
    , lastInfoRef :: IORef (Maybe (LogId, LogInfo))
    , idParents :: [LogId]
    }

getLastInfo :: MonadIO m => SloggerT m (Maybe (LogId, LogInfo))
getLastInfo = do
    ss <- SloggerT get
    liftIO $ readIORef (lastInfoRef ss)

setLastInfo :: MonadIO m => Maybe (LogId, LogInfo) -> SloggerT m ()
setLastInfo minfo = do
    ss <- SloggerT get
    liftIO $ writeIORef (lastInfoRef ss) minfo

getNextId :: MonadIO m => SloggerT m LogId
getNextId = do
    ss <- SloggerT get
    liftIO $ atomicModifyIORef (nextIdRef ss) (\i -> (i + 1, i))

persistData :: MonadIO m => LogData -> m (Maybe LogDataOffset)
persistData (LogData []) = return Nothing
persistData ld = liftIO $ fmap Just $ appendData "slog-data" ld

getIdParents :: Monad m => SloggerT m [LogId]
getIdParents = liftM idParents (SloggerT get)

setIdParents :: Monad m => [LogId] -> SloggerT m ()
setIdParents xs = do
    old <- SloggerT get
    SloggerT . put $ old { idParents = xs }

renderMetaData :: LogMetaData -> LogStr
renderMetaData lmd =
    " [slog id=" <> toLogStr (show (logId lmd)) <>
    " pid=" <> toLogStr (show (fromMaybe (-1) (logParentId lmd))) <>
    " offset=" <> toLogStr (show (fromMaybe (-1) (logDataOffset lmd))) <>
    "]"
