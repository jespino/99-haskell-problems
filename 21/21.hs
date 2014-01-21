insertAt :: a -> [a] -> Int -> [a]
insertAt a xs 1 = a:xs
insertAt a (x:xs) n = x:(insertAt a xs (n - 1))
