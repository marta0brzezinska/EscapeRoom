module GameTest where

    import Test.QuickCheck
    import Engine.Game
    import Engine.Item
    import Engine.Room
    import Engine.Action
    import ItemTest
    import ActionTest
    import RoomTest
    
    instance Arbitrary GameState where
        arbitrary = do
            rooms<-arbitrary
            items<-arbitrary
            actions<-arbitrary
            currRoom<-arbitrary
            info<-arbitrary
            return (GameState rooms items actions currRoom info)
    
    testAddItems::Int->[Item]->Bool
    testAddItems int items =  addItems int items == map(\(Item itemNumber itemName itDesc itemOwned) -> if int==itemNumber then Item itemNumber itemName itDesc (itemOwned+1) else Item itemNumber itemName itDesc itemOwned) items

    testCheckInventory::[Item]->Bool
    testCheckInventory items
        |null (filter (isOwnedInCopies 1) items) = checkInventory items == "Your inventory is empty. "
        |otherwise = checkInventory items == "Items in your inventory:" ++ itemsDescription (filter (isOwnedInCopies 1) items)

    testItemInfo::[Item]->Int->Bool
    testItemInfo items int
        |null (filter (isItemNumber int) items) = itemInfo items int == "No item no. "++ show int
        |otherwise = itemInfo items int == itemDesc (head (filter (isItemNumber int) items))

    testRoomInfo::[Room]->Int->Bool
    testRoomInfo rooms int
        |null (filter (isRoomNumber int) rooms) = roomInfo rooms int == "No room no. "++ show int
        |otherwise = roomInfo rooms int == roomDesc (head (filter (isRoomNumber int) rooms))    

    testUpdageGameState::Bool->Action->Int->[Room]->[Item]->Bool
    testUpdageGameState canCurrActionBeUsed currAction currRoom rooms items
        |actType currAction==0 = updageGameState canCurrActionBeUsed currAction currRoom rooms items == (currRoom, items, checkInventory items ++ "\n")
        |actType currAction==(-1) = updageGameState canCurrActionBeUsed currAction currRoom rooms items == (currRoom, items, "You cannot perform this action. \n")
        |canCurrActionBeUsed && actType currAction==1 = updageGameState canCurrActionBeUsed currAction currRoom rooms items == (currRoom, addItems (actEffect currAction) items, actSucc currAction ++ itemInfo items (actEffect currAction) ++ "\n")
        |canCurrActionBeUsed && actType currAction==2 = updageGameState canCurrActionBeUsed currAction currRoom rooms items == ((actEffect currAction), items, actSucc currAction ++ "\n" ++ roomInfo rooms (actEffect currAction) ++ "\n")
        |canCurrActionBeUsed && actType currAction==3 = updageGameState canCurrActionBeUsed currAction currRoom rooms items == (currRoom, items, actSucc currAction ++ itemInfo items (actEffect currAction) ++ "\n")
        |otherwise = updageGameState canCurrActionBeUsed currAction currRoom rooms items == (currRoom, items, actFail currAction ++ "\n")

    test:: IO()
    test = quickCheck testAddItems >> quickCheck testCheckInventory >> quickCheck testItemInfo >> quickCheck testRoomInfo >> quickCheck testUpdageGameState