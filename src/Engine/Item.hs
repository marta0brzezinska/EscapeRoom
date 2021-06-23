{-# LANGUAGE LambdaCase #-}
module Engine.Item where
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

    isntSemi x = x/=';'
    isntPoint x = x/='\n'

    pierwszy :: Parser String
    pierwszy = many (sat isntSemi)

    drugi:: Parser String
    drugi = many (sat isntPoint)

    itemParser = do
        n<-pierwszy
        modify(\(x:xs)->xs)
        id<-drugi
        return (Item n id)

    