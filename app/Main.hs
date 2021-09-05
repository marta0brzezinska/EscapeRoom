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

play = do
  act <- lift readAction
  info <- updateGame act
  unless (info == "You are outside") $ do -- if the distance is zero, the game halts
    play --otherwise, continue guessing. this loops the play function into calling itself until the player makes a correct guess

main :: IO ()
--main = evalStateT play (initializeGame initPair)
main =  do
  nern <- execStateT initializeGame (GameState [] [] [] 0 "") --initialize the game and write the current state into nern
  evalStateT play nern --pass the current state to a transformative function which will apply play to it until the game halts
  return ()
--3 END