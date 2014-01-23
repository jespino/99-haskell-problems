isPrime :: Int -> Bool
isPrime 1 = True
isPrime 2 = True
isPrime n = all (not . isPrime) [ x | x <- [2..(n-1)], mod n x == 0]
