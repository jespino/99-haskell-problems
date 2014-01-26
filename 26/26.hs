combinations :: Int -> [a] -> [[a]]
combinations _ [] = [[]]
combinations 1 (xs) = [ [x] | x <- xs]
combinations n (x:xs) = [ j:y | j <- (x:xs), y <- (combinations (n - 1) xs) ]
