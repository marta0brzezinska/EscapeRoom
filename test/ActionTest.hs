module ActionTest where
    
    import Test.QuickCheck
    import Engine.Action
    import Engine.Item
    import ItemTest
    
    instance Arbitrary Action where
        arbitrary = do
            actDesc<-arbitrary
            actType<-arbitrary
            actReq<-arbitrary
            actSucc<-arbitrary
            actFail<-arbitrary
            actEffect<-arbitrary
            actUsed<-arbitrary
            return (Action actDesc actType actReq actSucc actFail actEffect actUsed)

    testIsActDesc::String->Action->Bool 
    testIsActDesc str act = isActDesc str act == (str == actDecs act)

    testCanBeUsed::[Item]->Int->Action->Bool 
    testCanBeUsed items int act 
        | actType act==2 && any (isOwnedItem (actReq act)) items = canBeUsed items int act
        | (actType act==1 || actType act==3) && actReq act==int = canBeUsed items int act
        | otherwise = not (canBeUsed items int act)

    testUse::Action->Bool
    testUse act = used (use act) == 1

    test:: IO()
    test = quickCheck testIsActDesc >> quickCheck testCanBeUsed >> quickCheck testUse