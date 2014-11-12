{-# LANGUAGE TemplateHaskell #-}

module Demo2 where

import Control.Monad.Logger (runStdoutLoggingT)
import Slog

main :: IO ()
main = runStdoutLoggingT $ runSloggerT $ do
    $(slog "test 1")
    sfork $ do
        $(slog "test 2")
        $(slog "test 3")
        return ()
