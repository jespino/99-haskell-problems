myGCD :: Int -> Int -> Int
myGCD n m
        | m == 0 = abs n
        | otherwise = myGCD m (mod n m)
