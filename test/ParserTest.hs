module ParserTest where

    import Test.QuickCheck
    import Engine.Parser

    testIsntSemi :: Char -> Bool
    testIsntSemi x = isntSemi x == (x/=';')

    testIsntEnd :: Char -> Bool
    testIsntEnd x = isntEnd x == (x/='\n')
    
    testIsSemi :: Char -> Bool
    testIsSemi x = isSemi x == (x==';')

    testIsEnd :: Char -> Bool
    testIsEnd x = isEnd x == (x=='\n')

    test:: IO()
    test = quickCheck testIsntSemi >> quickCheck testIsntEnd >> quickCheck testIsSemi >> quickCheck testIsEnd