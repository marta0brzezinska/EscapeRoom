module RoomTest where
    
    import Test.QuickCheck
    import Engine.Room

    instance Arbitrary Room where
        arbitrary = do
            roomNumber<-arbitrary
            roomName<-arbitrary
            roomDesc<-arbitrary
            return (Room roomNumber roomName roomDesc)

    testIsRoomNumber::Int->Room->Bool 
    testIsRoomNumber int room = isRoomNumber int room ==(roomNumber room == int)

    test:: IO()
    test = quickCheck testIsRoomNumber