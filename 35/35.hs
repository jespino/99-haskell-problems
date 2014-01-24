isPrime :: Int -> Bool
isPrime 1 = True
isPrime 2 = True
isPrime n = all (not . isPrime) [ x | x <- [2..(n-1)], mod n x == 0]

primeFactors :: Int -> [Int]
primeFactors 1 = []
primeFactors n = factorsHead:(primeFactors (n `div` factorsHead))
        where factorsHead = head [ x | x <- [2..n], isPrime x, mod n x == 0 ]
