{-# LANGUAGE LambdaCase #-}
module Engine.Parser where
    import System.IO
    import Control.Monad.State.Strict
    import Control.Applicative

    type Parser a = StateT String [] a

    character :: Parser Char
    character = StateT $ \case
                        []     -> []
                        (x:xs) -> [(x,xs)]

    zero :: Parser a
    zero = mzero

    sat :: (Char -> Bool) -> Parser Char
    sat chi = do
        x <- character
        if chi x then return x else zero

    isntSemi x = x/=';'
    isntEnd x = x/='\n'
    isSemi = (==';')
    isEnd = (=='\n')



    


    