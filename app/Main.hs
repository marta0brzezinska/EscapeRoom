module Main where

import Lib
import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Trans.State.Strict
import System.Environment
import System.IO.Error
import Engine.ItemParser

main :: IO ()
main = do
    [str,strs] <- readItemFile
    return ()
