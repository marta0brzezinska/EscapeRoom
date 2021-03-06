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
    }

    -- | This is the definition of show function for data type Item
    instance Show Item where
         show (Item _ itemName itemDesc itemOwned) = itemName ++ ": " ++ itemDesc ++ " You have " ++ show itemOwned ++ " of them."

    -- | This is the definition of == function for data type Item
    instance Eq Item where 
        item1 == item2 = (itemNumber item1==itemNumber item2 && itemName item1==itemName item2 && itemDesc item1==itemDesc item2 && itemOwned item1==itemOwned item2)
    -- | This function reads the file containing Items data
    readItemFile :: IO String
    readItemFile = readFile "src/Data/Items.txt"

    -- | This function returns Parser of the number of the Item
    parseItemNumber:: Parser Int
    parseItemNumber = read <$> many (sat isntSemi)

    -- | This function returns Parser of the name of the Item
    parseItemName :: Parser String
    parseItemName = many (sat isntSemi)

    -- | This function returns Parser of the description of the Item
    parseItemDesc:: Parser String
    parseItemDesc = many (sat isntSemi)

    -- | This function returns Parser of the number of Items owned
    parseActionOwned:: Parser Int
    parseActionOwned = read <$> many (sat isntEnd)

    -- | This function returns Parser of the Item
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

    -- | This function returns Parser of the Item and skips the end of the line
    itemsParser :: Parser Item
    itemsParser = do
        x<-itemParser
        sat isEnd
        return x

    -- | This function returns the Parser of the list of the Items
    initializeItems :: Parser [Item]
    initializeItems = many itemsParser

    -- | This function returns full descriptions of items on the given list
    itemsDescription::[Item]->String
    itemsDescription [] = ""
    itemsDescription (item:items) = "\n" ++ show item ++ itemsDescription items

    -- | This function checks if the given integer is the number of the given Item and if the item is owned by the player
    isOwnedItem::Int->Item->Bool 
    isOwnedItem actReq (Item itemNumber _ _ itemOwned) = itemOwned>=1 && itemNumber==actReq

    -- | This function checks if the given integer is the number of the given Item
    isItemNumber::Int->Item->Bool 
    isItemNumber actEffect (Item itemNumber _ _ _) = itemNumber==actEffect

    -- | This function checks if actual Item is owned in at least int copies
    isOwnedInCopies::Int->Item->Bool
    isOwnedInCopies int (Item itemNumber _ _ itemOwned) = itemNumber>0 && itemOwned>=int