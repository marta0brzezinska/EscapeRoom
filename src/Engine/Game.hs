module Engine.Game where
    import System.IO
    import Control.Monad.State.Strict
    import Engine.Item
    import Engine.Room
    import Engine.Action

    data StanGry = StanGry{
        rooms::[Room],
        items::[Item],
        actions::[Action]
    }deriving (Show)

    type GameIO = StateT StanGry IO

    initializeGame::GameIO ()
    initializeGame = do
        roomsStr <- lift readItemFile
        itemsStr <- lift readItemFile
        actionsStr <- lift readItemFile
        modify(const(StanGry (fst $ head $ runStateT initializeRooms roomsStr) (fst $ head $ runStateT initializeItems itemsStr) (fst $ head $ runStateT initializeActions actionsStr)))


