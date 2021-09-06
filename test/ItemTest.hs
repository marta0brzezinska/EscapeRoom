module ItemTest where

    import Test.QuickCheck
    import Engine.Item
    
    instance Arbitrary Item where
        arbitrary = do
            itemNumber <- arbitrary
            itemName <- arbitrary
            itemDesc <- arbitrary
            itemOwned <- arbitrary
            return (Item itemNumber itemName itemDesc itemOwned)

    testIsOwnedItem::Int->Item->Bool
    testIsOwnedItem int it = isOwnedItem int it == (itemOwned it>=1 && itemNumber it==int)

    testIsItemNumber::Int->Item->Bool 
    testIsItemNumber int it = isItemNumber int it == (itemNumber it==int)

    testIsOwnedInCopies::Int->Item->Bool
    testIsOwnedInCopies int it = isOwnedInCopies int it == (itemNumber it >0 && itemOwned it>=int)

    test:: IO()
    test = quickCheck testIsOwnedItem >> quickCheck testIsItemNumber >> quickCheck testIsOwnedInCopies
