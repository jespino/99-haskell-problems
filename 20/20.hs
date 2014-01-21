removeAt :: Int -> [a] -> (a, [a])
removeAt n xs = (xs !! (n - 1), removeN n xs)
        where removeN 1 (x:xs) = xs
              removeN n (x:xs) = x:(removeN (n - 1) xs)
