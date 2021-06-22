{-# LANGUAGE LambdaCase #-}
module Engine.ItemParser where
    import Engine.Game
    import System.IO
    import Control.Monad.State.Strict

    readItemFile :: IO [String]
    readItemFile = lines <$> readFile "src/Data/Items.txt"

    type Parser a = StateT String [] a

    letter :: Parser Char
    letter = StateT $ \case 
                        []     -> []
                        (x:xs) -> [(x,xs)]

    zero :: Parser a
    zero = mzero

    sat :: (Char -> Bool) -> Parser Char
    sat chi = do
        x <- letter
        if chi x then return x else zero

    isLetter x = x/=';'

    
