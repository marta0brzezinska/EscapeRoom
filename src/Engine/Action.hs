{-# LANGUAGE LambdaCase #-}
module Engine.Action where
    import System.IO
    import Control.Monad.State.Strict
    import Control.Applicative
    import Engine.Parser
    import Engine.Item

    -- | This is data type representing an action in the game
    data Action = Action{
        -- | This is the description that identifies the Action
        actDecs::String,
        -- | This is the type of the Action: 1-Item related 2-Room related
        actType::Int,
        -- | This is the number that identifies Room required to perform the Action
        actRoomReq::Int,
        -- | This is the number that identifies Item required to perform the Action
        actItemReq::Int,
        -- | This is the description of the succesfull performance of the Action
        actSucc::String,
        -- | This is the description of the failed performance of the Action
        actFail::String,
        -- | This is the number that describes the effect of the Action
        actEffect::Int,
        -- | This is the number describing how many more times the Action can been performed
        actUsed::Int 
    }deriving(Show)
    
    -- | This is the definition of == function for data type Action
    instance Eq Action where 
        (Action actDecs1 _ _ _ _ _ _ _)==(Action actDecs2 _ _ _ _ _ _ _) = actDecs1==actDecs2

    -- | This function reads the file containing Actions data
    readActionFile :: IO String
    readActionFile = readFile "src/Data/Actions.txt"

    -- | This function returns Parser of fields of the Action that are strings
    parseActionString :: Parser String
    parseActionString = many (sat isntSemi)

    -- | This function returns Parser of fields of the Action that are integers
    parseActionInt :: Parser Int
    parseActionInt = read <$> many (sat isntSemi)

    -- | This function returns Parser of the number describing whether or not the Action has been performed
    parseActionUsed:: Parser Int
    parseActionUsed = read <$> many (sat isntEnd)

    -- | This function returns Parser of the Action
    actionParser :: Parser Action
    actionParser = do
        actDesc<-parseActionString
        sat isSemi
        actType<-parseActionInt
        sat isSemi
        actRoomReq<-parseActionInt
        sat isSemi
        actItemReq<-parseActionInt
        sat isSemi
        actSucc<-parseActionString
        sat isSemi
        actFail<-parseActionString
        sat isSemi
        actEffect<-parseActionInt
        sat isSemi
        actUsed<-parseActionUsed
        return (Action actDesc actType actRoomReq actItemReq actSucc actFail actEffect actUsed)

    -- | This function returns Parser of the Action and skips the end of the line
    actionsParser :: Parser Action
    actionsParser = do
        x<-actionParser
        sat isEnd
        return x

    -- | This function returns Parser of the list of the Actions
    initializeActions :: Parser [Action]
    initializeActions = many actionsParser

    -- | This function checks if the given string is the description of the given Action
    isActDesc::String->Action->Bool 
    isActDesc str (Action actDecs _ _ _ _ _ _ _) = str == actDecs

    -- | This function determines whether or not the given Action can be performed
    canBeUsed::[Item]->Int->Action->Bool 
    canBeUsed items currRoom (Action _ actType actRoomReq actItemReq _ _ _ actUsed)
        |(actType>0) && any (isOwnedItem actItemReq) items && actRoomReq==currRoom && actUsed>0 = True
        |otherwise = False

    -- | This function decreases the actUsed argument of the Action by 1 when the action has been performed succesfully
    use:: Action -> Action
    use (Action actDesc actType actRoomReq actItemReq actSucc actFail actEffect actUsed) = Action actDesc actType actRoomReq actItemReq actSucc actFail actEffect (actUsed-1)