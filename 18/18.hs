slice :: [a] -> Int -> Int -> [a]
slice xs 1 0 = []
slice (x:xs) 1 n = x:(slice xs 1 (n - 1))
slice (x:xs) n m = slice xs (n - 1) (m - 1)
