elementAt :: [a] -> Int -> a
elementAt (x:xs) 1 = x
elementAt (x:xs) z = elementAt xs (z - 1)
