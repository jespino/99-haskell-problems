range :: Int -> Int -> [Int]
range n m
        | n == m = [m]
        | otherwise = n:(range (n + 1) m)
