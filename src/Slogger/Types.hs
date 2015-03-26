{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveFunctor #-}

module Slogger.Types
    ( LogMetadata(..)
    , AnnotatedLog(..)
    , LogAnn(..)
    , AnnName
    , unAnnName
    , mkAnnName
    , unsafeMkAnnName
    , ISO8601(..)
    , LogSpan(..)
    , LogStr(..)
    , fromLogStr
    , toLogStr
    -- Re-exports
    , Loc(..)
    , LogLevel
    , LogSource
    ) where

import           Control.Applicative
import           Control.Monad.Logger (LogLevel, LogSource)
import           Data.Aeson
import           Data.ByteString (ByteString)
import           Data.Data (Data)
import           Data.Function (on)
import qualified Data.HashMap.Strict as HM
import           Data.Maybe (mapMaybe)
import           Data.Monoid (Monoid)
import           Data.String (IsString)
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Time (UTCTime)
import           Data.Time.ISO8601 (formatISO8601, parseISO8601)
import           Data.Typeable (Typeable)
import           GHC.Generics (Generic)
import           Language.Haskell.TH.Instances ()
import           Language.Haskell.TH.Syntax (Loc(..))
import qualified System.Log.FastLogger as FL

--TODO: NFData instances?

data LogMetadata = LogMetadata
    { logLoc    :: !Loc
    , logSource :: !LogSource
    , logLevel  :: !LogLevel
    } deriving (Eq, Ord, Show, Generic, Typeable)

-- Annotated log messages

data AnnotatedLog ann = AnnotatedLog
    { logTags      :: ![Text]
    , logMsg       :: !Text
    , logAnns      :: ![LogAnn ann]
    } deriving (Eq, Ord, Show, Generic, Typeable)

instance ToJSON mark => ToJSON (AnnotatedLog mark) where
    toJSON AnnotatedLog {..} =
        Object $
        (if null logTags then id else HM.insert "tags" (toJSON logTags)) $
        (if T.null logMsg then id else HM.insert "msg" (toJSON logMsg)) $
        HM.fromList (map annToPair logAnns)
      where
        annToPair x = unAnnName (annName x) .= object
            (maybe id ((:) . ("span" .=)) (annSpan x) $
                [ "type" .= annType x
                , "value" .= annValue x
                ])

instance FromJSON mark => FromJSON (AnnotatedLog mark) where
    parseJSON = withObject "AnnotatedLog" $ \v -> AnnotatedLog <$>
        v .:? "tags" .!= [] <*>
        v .:? "msg" .!= "" <*>
        sequence (mapMaybe annFromPair (HM.toList v))
      where
        annFromPair (k, v) =
            case mkAnnName k of
                Nothing -> Nothing
                Just name -> Just $ flip (withObject "LogAnn") v $ \o ->
                    LogAnn <$>
                    pure name <*>
                    o .:? "span" <*>
                    o .: "type" <*>
                    o .: "value"

data LogAnn mark = LogAnn
    { annName  :: !AnnName
    , annSpan  :: !(Maybe LogSpan)
    , annType  :: !Text
    , annValue :: !mark
    } deriving (Eq, Ord, Show, Generic, Data, Typeable, Functor)

newtype AnnName = AnnName Text
    deriving (Eq, Ord, Show, Generic, Data, Typeable)

unAnnName :: AnnName -> Text
unAnnName (AnnName name) = name

mkAnnName :: Text -> Maybe AnnName
mkAnnName name
    | name `notElem` reservedAnnNames = Just (AnnName name)
    | otherwise = Nothing

unsafeMkAnnName :: Text -> AnnName
unsafeMkAnnName name
    | name `notElem` reservedAnnNames = AnnName name
    | otherwise = error $
        show name ++ " is a reserved AnnName. (" ++ show reservedAnnNames ++ ")"

reservedAnnNames :: [Text]
reservedAnnNames = ["tags", "msg"]

data LogSpan = LogSpan !Int !Int
    deriving (Eq, Ord, Show, Generic, Data, Typeable)

instance ToJSON LogSpan
instance FromJSON LogSpan

-- | Wrapper around UTCTime which uses ISO8601 for serialization.
-- For use as a timestamp field for
-- https://www.loggly.com/docs/automated-parsing/#json
newtype ISO8601 = ISO8601 { unISO8601 :: UTCTime }
    deriving (Eq, Ord)

instance ToJSON ISO8601 where
    toJSON = toJSON . formatISO8601 . unISO8601

instance FromJSON ISO8601 where
    parseJSON v = do
        mstr <- parseJSON v
        case parseISO8601 mstr of
            Nothing -> fail "Failed to parse ISO8601 timestamp"
            Just utc -> return (ISO8601 utc)

-- TODO: do we need this anymore? not using it...

-- | Wrapper around LogStr that additionally supports Eq, Ord, Show,
-- Generic, Typeable.
newtype LogStr = LogStr { unLogStr :: FL.LogStr }
    deriving (FL.ToLogStr, IsString, Monoid, Generic, Typeable)

fromLogStr :: LogStr -> ByteString
fromLogStr = FL.fromLogStr . unLogStr

toLogStr :: FL.ToLogStr a => a -> LogStr
toLogStr = LogStr . FL.toLogStr

instance Eq LogStr where
    (==) = (==) `on` fromLogStr
    (/=) = (/=) `on` fromLogStr

-- | Assumes OverloadedStrings, when interpreting this as code.
instance Show LogStr where
    showsPrec _ x = shows (fromLogStr x)

instance Ord LogStr where
    compare = compare `on` fromLogStr
