{-# LANGUAGE LambdaCase #-}
module Engine.Room where
    import System.IO
    import Control.Monad.State.Strict
    import Control.Applicative
    import Engine.Parser

    -- | This is data type representing a location in the game
    data Room = Room{
        -- | This is the number that identifies the Room
        roomNumber::Int,
        -- | This is the name of the Room
        roomName::String,
        -- | This is the description of the Room
        roomDesc::String
    }deriving(Show)

    -- | This function checks if the given integer is the number of the given Room
    (=****)::Int->Room->Bool 
    (=****) actEffect (Room roomNumber _ _) = roomNumber==actEffect
    
    -- | This function reads the file containing Rooms data
    readRoomFile :: IO String
    readRoomFile = readFile "src/Data/Rooms.txt"

    -- | This function parses the number of the Room
    parseRoomNumber :: Parser Int
    parseRoomNumber = read <$> many (sat isntSemi)

    -- | This function parses the name of the Room
    parseRoomName:: Parser String
    parseRoomName = many (sat isntSemi)

    -- | This function parses the description of the Room
    parseRoomDesc:: Parser String
    parseRoomDesc = many (sat isntEnd)

    -- | This function parses the Room
    itemParser :: Parser Room
    itemParser = do
        roomNumber<-parseRoomNumber
        sat isSemi
        roomName<-parseRoomName
        sat isSemi
        roomDesc<-parseRoomDesc
        return (Room roomNumber roomName roomDesc)

    -- | This function parses the Room and skips the end of the line
    itemsParser :: Parser Room
    itemsParser = do
        x<-itemParser
        sat isEnd
        return x

    -- | This function parses the list of the Room
    initializeRooms :: Parser [Room]
    initializeRooms = many itemsParser

    -- | This function returns the description of the given Room
    roomDescription::Room->String
    roomDescription (Room _ _ roomDesc) = roomDesc

    


    