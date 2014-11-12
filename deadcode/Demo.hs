{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module Demo where

import Control.Monad.Logger (runStdoutLoggingT)
import Slogger

main :: IO ()
main = runStdoutLoggingT $ evalSloggerT $ do
    $logNestDebug "nester" $ do
        $logDebug "test"
        $logDebug "testStart"
