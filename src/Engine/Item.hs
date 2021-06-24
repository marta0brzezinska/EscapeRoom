{-# LANGUAGE LambdaCase #-}
module Engine.Item where
    import System.IO
    import Control.Monad.State.Strict
    import Control.Applicative
    import Engine.Parser

    data Item = Item{
        name::String,
        itemDesc::String
    }deriving(Show)
    
    readItemFile :: IO String
    readItemFile = readFile "src/Data/Items.txt"

    parseItemName :: Parser String
    parseItemName = many (sat isntSemi)

    parseItemDesc:: Parser String
    parseItemDesc = many (sat isntEnd)

    itemParser :: Parser Item
    itemParser = do
        name<-parseItemName
        sat isSemi
        itDesc<-parseItemDesc
        return (Item name itDesc)

    itemsParser :: Parser Item
    itemsParser = do
        x<-itemParser
        sat isEnd
        return x

    initializeItems :: Parser [Item]
    initializeItems = many itemsParser

    


    