{-# LANGUAGE LambdaCase #-}
module Engine.Item where
    import System.IO
    import Control.Monad.State.Strict
    import Control.Applicative
    import Engine.Parser

    -- | This is data type representing an object in the game
    data Item = Item{
        -- | This is the number that identifies the Item
        itemNumber::Int,
        -- | This is the name of the Item
        itemName::String,
        -- | This is the description of the Item
        itemDesc::String,
        -- | This the number of Items owned
        itemOwned::Int 
    }deriving(Show)

    -- | This function checks if the given integer is the number of the given Item and if the item is owned by the player
    (=**)::Int->Item->Bool 
    (=**) actReq (Item itemNumber _ _ itemOwned) = itemOwned>=1 && itemNumber==actReq

    -- | This function checks if the given integer is the number of the given Item
    (=***)::Int->Item->Bool 
    (=***) actEffect (Item itemNumber _ _ _) = itemNumber==actEffect

    -- | This function reads the file containing Items data
    readItemFile :: IO String
    readItemFile = readFile "src/Data/Items.txt"

    -- | This function parses the number of the Item
    parseItemNumber:: Parser Int
    parseItemNumber = read <$> many (sat isntSemi)

    -- | This function parses the name of the Item
    parseItemName :: Parser String
    parseItemName = many (sat isntSemi)

    -- | This function parses the description of the Item
    parseItemDesc:: Parser String
    parseItemDesc = many (sat isntSemi)

    -- | This function parser the number of Items owned
    parseActionOwned:: Parser Int
    parseActionOwned = read <$> many (sat isntEnd)

    -- | This function parses the Item
    itemParser :: Parser Item
    itemParser = do
        itemNumber<-parseItemNumber
        sat isSemi
        itemName<-parseItemName
        sat isSemi
        itemDesc<-parseItemDesc
        sat isSemi
        itemOwned<-parseActionOwned
        return (Item itemNumber itemName itemDesc itemOwned)

    -- | This function parses the Item and skips the end of the line
    itemsParser :: Parser Item
    itemsParser = do
        x<-itemParser
        sat isEnd
        return x

    -- | This function parses the list of the Items
    initializeItems :: Parser [Item]
    initializeItems = many itemsParser

    -- | This function returns the description of the given Item
    itemDescription::Item->String
    itemDescription (Item _ _ itemDesc _) = itemDesc