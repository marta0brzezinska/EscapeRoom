{-# LANGUAGE LambdaCase #-}
module Engine.ItemParser where
    --import Engine.Game
    import System.IO
    import Control.Monad.State.Strict
    import Control.Applicative

    data Item = Item{
        name::String,
        itemDesc::String
    }deriving(Show)

    readItemFile :: IO [String]
    readItemFile = lines <$> readFile "src/Data/Items.txt"

    type Parser a = StateT String [] a

    character :: Parser Char
    character = StateT $ \case
                        []     -> []
                        (x:xs) -> [(x,xs)]

    zero :: Parser a
    zero = mzero

    sat :: (Char -> Bool) -> Parser Char
    sat chi = do
        x <- character
        if chi x then return x else zero

    isLetter x = x/=';' && x/='.'

    letter = sat isLetter

    stringi :: Parser String
    stringi = many letter

    itemParser = do
        n<-stringi
        id<-stringi
        return (Item n id)

        