module ItemsTest where

import Test.QuickCheck
import Control.Monad.Trans.Class
import System.IO
import Engine.Item

testReadItemFile::Int -> Bool 
testReadItemFile int= do 
    file <- readItemFile 
    file == "1;Key;It looks like it opens a door.;1\n2;Book;It smells really nice.;1\n3;TV;It looks old but it seems like it still works.;2"

test:: IO()
test = quickCheck testReadItemFile 

