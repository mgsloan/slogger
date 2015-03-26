{-# LANGUAGE OverloadedStrings #-}

module JsonExample where

import Control.Monad.Logger
import Slogger
import Slogger.Types

main :: IO ()
main = runStdoutLoggingT $ do
    logJson metadata [] "" ([] :: [LogAnn Int])
    logJson metadata [] "" [LogAnn (unsafeMkAnnName "x") Nothing "Int" (1 :: Int)]

metadata :: LogMetadata
metadata = LogMetadata
    (Loc "<unknown>" "<unknown>" "<unknown>" (0,0) (0,0))
    "JsonExample"
    LevelDebug
