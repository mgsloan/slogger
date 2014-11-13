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
    , slog'
    , snest
    , sfork
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
import Data.Monoid
import Language.Haskell.TH
import LogFunc
import Types

slog :: LogFunc a => a
slog = logFunc (LogFuncSettings { logJustTH = True }) []

slog' :: LogFunc a => a
slog' = logFunc (LogFuncSettings { logJustTH = False }) []

snest :: (MonadSlogger m, LogFunc (m ()), MonadMask m) => m a -> m a
snest f = do
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

sfork :: Q Exp
sfork = do
    loc <- location
    [| sforkInternal (Just $(liftLoc loc)) |]

sfork' :: (MonadSlogger m, LogFunc (m ()), MonadMask m, MonadBaseControl IO m) => m () -> m ()
sfork' = sforkInternal Nothing

sforkInternal :: (MonadSlogger m, LogFunc (m ()), MonadMask m, MonadBaseControl IO m) => Maybe Loc -> m () -> m ()
sforkInternal mloc f = do
    pidVar <- newEmptyMVar
    pid <- fork $ do
        pid <- takeMVar pidVar
        () <- case mloc of
            Just loc -> slog ("Forked thread " :: String) (show pid) mloc
            Nothing -> slog ("Forked thread " :: String) (show pid)
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

data SloggerState = SloggerState
    { nextIdRef :: IORef LogId
    , lastInfoRef :: IORef (Maybe (LogId, LogInfo))
    , idParents :: [LogId]
    }

instance (MonadLogger m, MonadIO m) => MonadSlogger (SloggerT m) where
    getLastInfo = do
        ss <- SloggerT get
        liftIO $ readIORef (lastInfoRef ss)
    setLastInfo minfo = do
        ss <- SloggerT get
        liftIO $ writeIORef (lastInfoRef ss) minfo
    getNextId = do
        ss <- SloggerT get
        liftIO $ atomicModifyIORef (nextIdRef ss) (\i -> (i + 1, i))
    getIdParents = liftM idParents (SloggerT get)
    setIdParents xs = do
        old <- SloggerT get
        SloggerT . put $ old { idParents = xs }

instance (MonadLogger m, MonadIO m) => LogFunc (SloggerT m ())
