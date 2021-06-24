module Engine.Game where
    import System.IO
    import Control.Monad.State.Strict
    import Engine.Item
    import Engine.Room
    import Engine.Action

    data StanGry = StanGry{
        room::Room,
        items::[Item],
        actions::[Action]
    }deriving (Show)

    type GameIO = StateT StanGry IO

    --initializeGame::GameIO ()
    --initializeGame = do
