{-# LANGUAGE LambdaCase #-}
module Engine.Action where
    import System.IO
    import Control.Monad.State.Strict
    import Control.Applicative
    import Engine.Parser

    data Action = Action{
        actDecs::String,
        used::Int 
    }deriving(Show)
    
    readActionFile :: IO String
    readActionFile = readFile "src/Data/Actions.txt"

    parseActionDesc :: Parser String
    parseActionDesc = many (sat isntSemi)

    parseActionUsed:: Parser Int
    parseActionUsed = read <$> many (sat isntEnd)

    actionParser :: Parser Action
    actionParser = do
        actDesc<-parseActionDesc
        sat isSemi
        actUsed<-parseActionUsed
        return (Action actDesc actUsed)

    actionsParser :: Parser Action
    actionsParser = do
        x<-actionParser
        sat isEnd
        return x

    initializeActions :: Parser [Action]
    initializeActions = many actionsParser

    


    