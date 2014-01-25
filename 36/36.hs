isPrime :: Int -> Bool
isPrime 1 = True
isPrime 2 = True
isPrime n = all (not . isPrime) [ x | x <- [2..(n-1)], mod n x == 0]

headGroup :: [Int] -> Int -> (Int, Int)
headGroup [x] n = (x, n)
headGroup (x:y:xs) n
               | x == y = headGroup (y:xs) (n + 1)
               | x /= y = (x, n)

primeFactors :: Int -> [Int]
primeFactors 1 = []
primeFactors n = factorsHead:(primeFactors (n `div` factorsHead))
        where factorsHead = head [ x | x <- [2..n], isPrime x, mod n x == 0 ]

prime_factors_mult :: Int -> [(Int, Int)]
prime_factors_mult 1 = []
prime_factors_mult n = factorsHead:(prime_factors_mult (n `div` ((fst factorsHead) ^ (snd factorsHead))))
        where factorsHead = headGroup (primeFactors n) 1

