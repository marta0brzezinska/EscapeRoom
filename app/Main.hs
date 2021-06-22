module Main where

import Lib
import Data.Game
import qualified Data.Map.Strict as M
import           Control.Monad
import Control.Monad.Trans.Class ( MonadTrans(lift) )
import           Control.Monad.Trans.State.Strict
import           System.Environment
import           System.IO.Error
import Engine.ItemParser

main :: IO ()
main = do
    [str,strs] <- readItemFile
    return ()
