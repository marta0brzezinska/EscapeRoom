module Main where

import Lib
import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Trans.State.Strict
import System.Environment
import System.IO.Error
import Engine.Item
import Engine.Room
import Engine.Action
import Engine.Game

-- | This function represents the round of the game and loops if the message does not indicate the end of the gameopen door
play = do
  act <- lift readAction
  info <- updateGame act
  unless (info == "You are outside.") $ do
    play

-- | This is the main function that starts the game
main :: IO ()
main =  do
  firstState <- execStateT initializeGame (GameState [] [] [] 0 "")
  evalStateT play firstState
  return ()