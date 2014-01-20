split :: [a] -> Int -> ([a], [a])
split xs n = (takeN xs n, dropN xs n)
    where takeN xs 0 = []
          takeN (x:xs) n = x:(takeN xs (n - 1))
          dropN xs 0 = xs
          dropN (x:xs) n = (dropN xs (n - 1))
