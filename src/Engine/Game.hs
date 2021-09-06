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

    -- | This function initializes the game based on the game files and prints the description of the first location
    initializeGame::GameIO ()
    initializeGame = do
        roomsStr <- lift readRoomFile
        itemsStr <- lift readItemFile
        actionsStr <- lift readActionFile
        let rooms = fst $ head $ runStateT initializeRooms roomsStr
        let items = fst $ head $ runStateT initializeItems itemsStr
        let actions = fst $ head $ runStateT initializeActions actionsStr
        modify(const(GameState rooms items actions 1 ""))
        liftIO $ putStrLn "Game starts now."
        let currRoom = head (filter (isRoomNumber 1) rooms)
        let info = roomDesc currRoom
        liftIO $ putStrLn info

    -- | This function reads description of the action from the user
    readAction:: IO String
    readAction = do
        putStrLn "What do you do?"
        getLine

    -- | This function changes the number of owned items in the given list of items based on the given number of the item
    addItems::Int->[Item]->[Item]
    addItems actEffect items =  map(\(Item itemNumber itemName itDesc itemOwned) -> if actEffect==itemNumber then Item itemNumber itemName itDesc (itemOwned+1) else Item itemNumber itemName itDesc itemOwned) items

    -- | This function returns the description of the players inventory
    checkInventory::[Item]->String
    checkInventory items = do
            let itemsOwned = filter (isOwnedInCopies 1) items
            if null itemsOwned then "Your inventory is empty. " else "Items in your inventory:" ++ itemsDescription itemsOwned

    -- | This function returns description of the item from the given list of items based on the given number of the item
    itemInfo::[Item]->Int->String
    itemInfo items actEffect = do
        let currItems = filter (isItemNumber actEffect) items
        if null currItems then "No item no. "++ show actEffect else itemDesc (head currItems)

    -- | This function returns description of the room from the given list of rooms based on the given number of the room
    roomInfo::[Room]->Int->String
    roomInfo rooms actEffect = do
        let currRooms = filter (isRoomNumber actEffect) rooms
        if null currRooms then "No room no. "++ show actEffect else roomDesc (head currRooms)

    -- | This function updates the currRoom, items and info arguments based on whether or not the Action can be used, the given Action, currRoom, rooms and items
    updageGameState::
        -- | Bool argument saying whether or not the Action can be used
        Bool->
        -- | Action that player wants to perform
        Action->
        -- | Int representing the number of the current location
        Int->
        -- | list of game locations
        [Room]->
        -- | list of game items
        [Item]->
        -- | returned updated values of currRoom, items and info
        (Int, [Item], String)
    updageGameState canCurrActionBeUsed (Action _ actType _ actSucc actFail actEffect _) currRoom rooms items
        |actType==0 = (currRoom, items, checkInventory items ++ "\n")
        |actType==(-1) = (currRoom, items, "You cannot perform this action. \n")
        |canCurrActionBeUsed && actType==1 = (currRoom, addItems actEffect items, actSucc ++ itemInfo items actEffect ++ "\n")
        |canCurrActionBeUsed && actType==2 = (actEffect, items, actSucc ++ "\n" ++ roomInfo rooms actEffect ++ "\n")
        |canCurrActionBeUsed && actType==3 = (currRoom, items, actSucc ++ itemInfo items actEffect ++ "\n")
        |otherwise = (currRoom, items, actFail ++ "\n")

    -- | This function changes the state of the game based on the given description of the action
    makeAction:: String-> GameState -> GameState
    makeAction act (GameState rooms items actions currRoom _) = 
        GameState rooms newItems newActions newCurrRoom newInfo where
            currActions = filter (isActDesc act) actions
            currAction = if null currActions then Action "" (-1) 0 "" "" 0 0 else head currActions
            newActions = map (\action -> if currAction == action && canBeUsed items currRoom action then use action else action) actions
            canCurrActionBeUsed = canBeUsed items currRoom currAction
            (newCurrRoom, newItems, newInfo) = updageGameState canCurrActionBeUsed currAction currRoom rooms items
    
    -- | This function updates the state of the game and returns info displayed to the user
    updateGame :: String -> GameIO String
    updateGame act = do
        modify (makeAction act)
        (GameState rooms items actions currRoom info) <- get
        lift $ putStr info
        return info
