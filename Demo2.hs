{-# LANGUAGE TemplateHaskell #-}

module Demo2 where

import Control.Monad.Logger
import Slog
import System.IO
import Control.Monad.IO.Class

main :: IO ()
main = runStdoutLoggingT $ runSloggerT $ do
    $(slog LevelWarn "test 1")
    $sfork $ do
        let x = 10 :: Int
        $(slog "test 2") " " x
        $(slog "test 3")
        -- TODO: maybe runStdoutLoggingT should force this?
        liftIO $ hFlush stdout
        return ()
