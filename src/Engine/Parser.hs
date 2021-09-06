{-# LANGUAGE LambdaCase #-}
module Engine.Parser where
    import System.IO
    import Control.Monad.State.Strict
    import Control.Applicative

    -- | This is the definition of a Parser
    type Parser a = StateT String [] a

    -- | This function returns Parser of the first character
    character :: Parser Char
    character = StateT $ \case
                        []     -> []
                        (x:xs) -> [(x,xs)]

    -- | This function returns Parser of the first character if it fulfills the given condition
    sat :: (Char -> Bool) -> Parser Char
    sat chi = do
        x <- character
        if chi x then return x else mzero
        
    -- | This function checks if the given character isn't a semicolon
    isntSemi :: Char -> Bool
    isntSemi x = x/=';'

    -- | This function checks if the given character isn't an end of the line
    isntEnd :: Char -> Bool
    isntEnd x = x/='\n'
    
    -- | This function checks if the given character isn a semicolon
    isSemi :: Char -> Bool
    isSemi = (==';')

    -- | This function checks if the given character is an end of the line
    isEnd :: Char -> Bool
    isEnd = (=='\n')



    


    