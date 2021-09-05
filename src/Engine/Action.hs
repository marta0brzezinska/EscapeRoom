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
        -- | This is the number that identifies Item/Room required to perform the Action
        actReq::Int,
        -- | This is the description of the succesfull performance of the Action
        actSucc::String,
        -- | This is the description of the failed performance of the Action
        actFail::String,
        -- | This is the number that describes the effect of the Action
        actEffect::Int,
        -- | This is the number describing whether or not the Action has been performed
        used::Int 
    }deriving(Show)
    
    -- | This function checks if the given string is the description of the given Action
    (=*)::String->Action->Bool 
    (=*) str (Action act _ _ _ _ _ _) = str == act

    -- | This function reads the file containing Actions data
    readActionFile :: IO String
    readActionFile = readFile "src/Data/Actions.txt"

    -- | This function parses fields of the Action that are strings
    parseActionString :: Parser String
    parseActionString = many (sat isntSemi)

    -- | This function parses fields of the Action that are integers
    parseActionInt :: Parser Int
    parseActionInt = read <$> many (sat isntSemi)

    -- | This function parses the number describing whether or not the Action has been performed
    parseActionUsed:: Parser Int
    parseActionUsed = read <$> many (sat isntEnd)

    -- | This function parses the Action
    actionParser :: Parser Action
    actionParser = do
        actDesc<-parseActionString
        sat isSemi
        actType<-parseActionInt
        sat isSemi
        actReq<-parseActionInt
        sat isSemi
        actSucc<-parseActionString
        sat isSemi
        actFail<-parseActionString
        sat isSemi
        actEffect<-parseActionInt
        sat isSemi
        actUsed<-parseActionUsed
        return (Action actDesc actType actReq actSucc actFail actEffect actUsed)

    -- | This function parses the Action and skips the end of the line
    actionsParser :: Parser Action
    actionsParser = do
        x<-actionParser
        sat isEnd
        return x

    -- | This function parses the list of the Actions
    initializeActions :: Parser [Action]
    initializeActions = many actionsParser

    -- | This function determines whether or not the given Action can be performed based on the actReq argument
    canBeUsed::[Item]->Int->Action->Bool 
    canBeUsed items currRoom (Action _ actType actReq _ _ _ _)
        | actType==1 && any (actReq =**) items = True 
        | actType==2 && actReq==currRoom = True 
        | otherwise = False

    -- | This function changes the actUsed argument of the Action to 1 meaning the action has been performed succesfully
    use:: Action -> Action
    use (Action actDesc actType actReq actSucc actFail actEffect actUsed) = Action actDesc actType actReq actSucc actFail actEffect 1
    
    -- | This function returns the type of the given Action
    actionType::Action ->Int
    actionType (Action _ actType _ _ _ _ _) = actType

    