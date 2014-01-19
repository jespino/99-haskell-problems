repli :: [a] -> Int -> [a]
repli [] _ = []
repli (x:xs) y = (repeat x y) ++ (repli xs y)
    where
        repeat x 0 = []
        repeat x n = x:(repeat x (n - 1))
