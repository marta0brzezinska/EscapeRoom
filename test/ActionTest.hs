module ActionTest where
    
    import Test.QuickCheck
    import Engine.Action
    import Engine.Item
    import ItemTest
    
    instance Arbitrary Action where
        arbitrary = do
            actDesc<-arbitrary
            actType<-arbitrary
            actRoomReq<-arbitrary
            actItemReq<-arbitrary
            actSucc<-arbitrary
            actFail<-arbitrary
            actEffect<-arbitrary
            actUsed<-arbitrary
            return (Action actDesc actType actRoomReq actItemReq actSucc actFail actEffect actUsed)

    testIsActDesc::String->Action->Bool 
    testIsActDesc str act = isActDesc str act == (str == actDecs act)

    testCanBeUsed::[Item]->Int->Action->Bool 
    testCanBeUsed items int act 
        | (actType act>0) && any (isOwnedItem (actItemReq act)) items && actRoomReq act==int && actUsed act>0 = canBeUsed items int act
        | otherwise = not (canBeUsed items int act)

    testUse::Action->Bool
    testUse act = actUsed (use act) == actUsed act-1

    test:: IO()
    test = quickCheck testIsActDesc >> quickCheck testCanBeUsed >> quickCheck testUse