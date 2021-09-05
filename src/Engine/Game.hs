module Engine.Game where
    import System.IO
    import Control.Monad.State.Strict
    import Engine.Item
    import Engine.Room
    import Engine.Action

    -- | This is data type representing a game
    data GameState = GameState{
        -- | This is the list of locations available in the game
        rooms::[Room],
        -- | This is the list of objects available in the game
        items::[Item],
        -- | This is the list of actions available in the game
        actions::[Action],
        -- | This is the number of the current location in the game
        currRoom::Int,
        -- | This is the message printed by the game
        info::String
    }deriving (Show)

    type GameIO = StateT GameState IO

    -- | This function initializes the game based on the game files
    initializeGame::GameIO ()
    initializeGame = do
        roomsStr <- lift readItemFile
        itemsStr <- lift readItemFile
        actionsStr <- lift readItemFile
        modify(const(GameState (fst $ head $ runStateT initializeRooms roomsStr) (fst $ head $ runStateT initializeItems itemsStr) (fst $ head $ runStateT initializeActions actionsStr) 1 ""))
        liftIO $ putStrLn "Game starts now."

    -- | This function reads description of the action from the user
    readAction:: IO String
    readAction = do
        putStrLn "What do you do?"
        readLn

    -- | This function changes the number of owned items in the given list of items based on the given number of the item
    addItems::Int->[Item]->[Item]
    addItems actEffect items =  map(\(Item itemNumber itemName itDesc itemOwned) -> if actEffect==itemNumber then Item itemNumber itemName itDesc (itemOwned+1) else Item itemNumber itemName itDesc itemOwned) items

    -- | This function returns description of the item from the given list of items based on the given number of the item
    itemInfo::[Item]->Int->String
    itemInfo items actEffect = do
        let currItem = head (filter (actEffect =***) items)
        itemDescription currItem

    -- | This function returns description of the room from the given list of rooms based on the given number of the room
    roomInfo::[Room]->Int->String
    roomInfo rooms actEffect = do
        let currRoom = head (filter (actEffect =****) rooms)
        roomDescription currRoom

    -- | This function updates the currRoom, items and info arguments based on whether or not the Action can be used, the given Action, currRoom, rooms and items
    updageGameState::Bool->Action->Int->[Room]->[Item]->(Int, [Item], String)
    updageGameState canCurrActionBeUsed (Action _ actType _ actSucc actFail actEffect _) currRoom rooms items
        |canCurrActionBeUsed && actType==1 = (currRoom, addItems actEffect items, actSucc ++ "/n" ++ itemInfo items actEffect)
        |canCurrActionBeUsed && actType==2 = (actEffect, items, actSucc ++ "/n" ++ roomInfo rooms actEffect)
        |otherwise = (currRoom, items, actFail)

    -- | This function changes the state of the game based on the given description of the action
    makeAction:: String-> GameState -> GameState
    makeAction act (GameState rooms items actions currRoom info) = 
        GameState rooms newItems newActions newCurrRoom newInfo where
            newActions = map (\action -> if act =* action && canBeUsed items currRoom action then use action else action) actions
            currAction = head (filter (act =*) actions)
            canCurrActionBeUsed = canBeUsed items currRoom currAction
            (newCurrRoom, newItems, newInfo) = updageGameState canCurrActionBeUsed currAction currRoom rooms items
    
    -- | This function updates the state of the game and returns info to be displayed to the user
    updateGame :: String -> GameIO String
    updateGame act = do
        modify (makeAction act)
        (GameState rooms items actions currRoom info) <- get
        return info
