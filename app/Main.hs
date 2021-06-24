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
import Engine.Game

main :: IO ()
main = do
    --runStateT initializeGame
    return ()
