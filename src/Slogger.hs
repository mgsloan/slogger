{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

module Slogger where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Logger (MonadLogger, MonadLoggerIO, monadLoggerLog)
import Data.Aeson (ToJSON(..), encode)
import Data.Text (Text)
import Data.Time (getCurrentTime)
import Slogger.Types

logJson :: (MonadLoggerIO m, ToJSON mark) => LogMetadata -> [Text] -> Text -> [LogAnn mark] -> m ()
logJson metadata logTags logMsg logAnns' = do
    timestamp <- liftIO getCurrentTime
    let timestampAnn = LogAnn
            { annName = unsafeMkAnnName "timestamp"
            , annSpan = Nothing
            , annType = "ISO8601"
            , annValue = toJSON timestamp
            }
        logAnns = timestampAnn : map (fmap toJSON) logAnns'
    logJson' metadata AnnotatedLog {..}

logJson' :: (MonadLogger m, ToJSON mark) => LogMetadata -> AnnotatedLog mark -> m ()
logJson' LogMetadata {..} =
    monadLoggerLog logLoc logSource logLevel . encode
