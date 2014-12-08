{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
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

slog :: LogFunc a => a
slog = logFunc []

snest :: MonadSlogger m => m a -> m a
snest = sloggerNest

sfork :: Q Exp
sfork = do
    loc <- location
    [| sforkInternal (Just $(liftLoc loc)) |]

sfork' :: (MonadSlogger m, MonadMask m, MonadBaseControl IO m)
       => m () -> m ()
sfork' = sforkInternal Nothing

sforkInternal :: forall m. (MonadSlogger m, MonadMask m, MonadBaseControl IO m)
              => Maybe Loc -> m () -> m ()
sforkInternal mloc f = do
    pidVar <- newEmptyMVar
    pid <- fork $ do
        pid <- takeMVar pidVar
        defaultLogFunc [ toLogChunk $ "Forked thread " ++ show pid
                       , maybe LogEmpty toLogChunk mloc
                       ]
        snest f
    putMVar pidVar pid

runSloggerT :: MonadIO m => SloggerT m a -> m a
runSloggerT (SloggerT m) = do
    nextIdRef <- liftIO $ newIORef 1
    let lastInfo = Nothing
        idParents = []
    evalStateT m SloggerState {..}

newtype SloggerT m a = SloggerT { unSloggerT :: StateT SloggerState m a }
    deriving ( Functor, Applicative, Monad, MonadFix, MonadPlus, Alternative
             , MonadTrans, MonadIO, MonadLogger, MonadMask, MonadThrow
             , MonadCatch)

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
        let metaData = renderMetadata $ LogMetadata lid (headMay parents) moffset
        monadLoggerLog loc source level (str <> metaData)
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
    , lastInfo :: Maybe (LogId, LogInfo)
    , idParents :: [LogId]
    }

getLastInfo :: Monad m => SloggerT m (Maybe (LogId, LogInfo))
getLastInfo = SloggerT (gets lastInfo)

setLastInfo :: MonadIO m => Maybe (LogId, LogInfo) -> SloggerT m ()
setLastInfo minfo = SloggerT $ modify $ \old -> old { lastInfo = minfo }

getIdParents :: Monad m => SloggerT m [LogId]
getIdParents = SloggerT (gets idParents)

setIdParents :: Monad m => [LogId] -> SloggerT m ()
setIdParents xs = SloggerT $ modify $ \old -> old { idParents = xs }

getNextId :: MonadIO m => SloggerT m LogId
getNextId = do
    ref <- SloggerT (gets nextIdRef)
    liftIO $ atomicModifyIORef ref (\i -> (i + 1, i))

persistData :: MonadIO m => LogData -> m (Maybe LogDataOffset)
persistData (LogData []) = return Nothing
persistData ld = liftIO $ fmap Just $ appendData "slog-data" ld

renderMetadata :: LogMetadata -> LogStr
renderMetadata lmd =
    " [slog id=" <> toLogStr (show (logId lmd)) <>
    " pid=" <> toLogStr (show (fromMaybe (-1) (logParentId lmd))) <>
    " offset=" <> toLogStr (show (fromMaybe (-1) (logDataOffset lmd))) <>
    "]"
