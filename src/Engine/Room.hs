{-# LANGUAGE LambdaCase #-}
module Engine.Room where
    import System.IO
    import Control.Monad.State.Strict
    import Control.Applicative
    import Engine.Parser

    data Room = Room{
        number::Int,
        roomDesc::String
    }deriving(Show)
    
    readRoomFile :: IO String
    readRoomFile = readFile "src/Data/Rooms.txt"

    parseRoomNumber :: Parser Int
    parseRoomNumber = read <$> many (sat isntSemi)

    parseRoomDesc:: Parser String
    parseRoomDesc = many (sat isntEnd)

    itemParser :: Parser Room
    itemParser = do
        number<-parseRoomNumber
        sat isSemi
        roomDesc<-parseRoomDesc
        return (Room number roomDesc)

    itemsParser :: Parser Room
    itemsParser = do
        x<-itemParser
        sat isEnd
        return x

    initializeRooms :: Parser [Room]
    initializeRooms = many itemsParser

    


    