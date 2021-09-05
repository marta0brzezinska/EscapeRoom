module Engine.Game where
    import System.IO
    import Control.Monad.State.Strict
    import Engine.Item
    import Engine.Room
    import Engine.Action

    data GameState = GameState{
        rooms::[Room],
        items::[Item],
        actions::[Action],
        currRoom::Int,
        info::String
    }deriving (Show)

    type GameIO = StateT GameState IO

    initializeGame::GameIO ()
    initializeGame = do
        roomsStr <- lift readItemFile
        itemsStr <- lift readItemFile
        actionsStr <- lift readItemFile
        modify(const(GameState (fst $ head $ runStateT initializeRooms roomsStr) (fst $ head $ runStateT initializeItems itemsStr) (fst $ head $ runStateT initializeActions actionsStr) 1 ""))
        liftIO $ putStrLn "Game starts."

    readAction:: IO String
    readAction = do
        putStrLn "What do you do?"
        readLn

    addItems::Int->[Item]->[Item]
    addItems actEffect items =  map(\(Item itemNumber itemName itDesc itemOwned) -> if actEffect==itemNumber then Item itemNumber itemName itDesc (itemOwned+1) else Item itemNumber itemName itDesc itemOwned) items

    itemInfo::[Item]->Int->String
    itemInfo items actEffect = do
        let currItem = head (filter (actEffect =***) items)
        itemDescription currItem

    roomInfo::[Room]->Int->String
    roomInfo rooms actEffect = do
        let currRoom = head (filter (actEffect =****) rooms)
        roomDescription currRoom

    updageGameState::Bool->Action->Int->[Room]->[Item]->(Int, [Item], String)
    updageGameState canCurrActionBeUsed (Action _ actType _ actSucc actFail actEffect _) currRoom rooms items
        |canCurrActionBeUsed && actType==1 = (currRoom, addItems actEffect items, actSucc ++ "/n" ++ itemInfo items actEffect)
        |canCurrActionBeUsed && actType==2 = (actEffect, items, actSucc ++ "/n" ++ roomInfo rooms actEffect)
        |otherwise = (currRoom, items, actFail)

    makeAction:: String-> GameState -> GameState
    makeAction act (GameState rooms items actions currRoom info) = 
        GameState rooms newItems newActions newCurrRoom newInfo where
            newActions = map (\action -> if act =* action && canBeUsed items currRoom action then use action else action) actions
            currAction = head (filter (act =*) actions)
            canCurrActionBeUsed = canBeUsed items currRoom currAction
            (newCurrRoom, newItems, newInfo) = updageGameState canCurrActionBeUsed currAction currRoom rooms items
            
    updateGame :: String -> GameIO String
    updateGame act = do
        modify (makeAction act)
        (GameState rooms items actions currRoom info) <- get
        return info
