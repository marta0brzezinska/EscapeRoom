module Main where

import Lib
import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Trans.State.Strict ( StateT(runStateT) )
import System.Environment
import System.IO.Error
import Engine.Item
import Engine.Room
import Engine.Action

main :: IO ()
main = do
    str <- readItemFile
    print $ fst $ head $ runStateT initializeItems str
    str2 <- readRoomFile
    print $ fst $ head $ runStateT initializeRooms str2
    str3 <- readActionFile
    print $ fst $ head $ runStateT initializeActions str3
    return ()
