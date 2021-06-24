module Main where

import Lib
import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Trans.State.Strict ( StateT(runStateT) )
import System.Environment
import System.IO.Error
import Engine.Item

main :: IO ()
main = do
    str <- readItemFile
    print $ fst $ head $ runStateT initializeItems str
    return ()
