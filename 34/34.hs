coprime :: Int -> Int -> Bool
coprime n m = gcd n m == 1

totient :: Int -> Int
totient n = length [ x | x <- [1..n], coprime 10 x]
