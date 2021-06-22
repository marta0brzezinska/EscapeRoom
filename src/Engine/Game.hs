module Engine.Game where
    import System.IO
    import Control.Monad.State.Strict

    data Item = Item{
        name::String,
        itemDesc::String
    }deriving(Show)

    data Room = Room{
        number::Int,
        roomDesc::String
    }deriving(Show)

    data Action = Action{
        actDecs::String,
        used::Bool 
    }deriving(Show)

    data StanGry = StanGry{
        room::Room,
        items::[Item],
        actions::[Action]
    }deriving (Show)

    type GameIO = StateT StanGry IO

    --initializeGame::GameIO ()
    --initializeGame = do
