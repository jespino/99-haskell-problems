pack :: (Eq a) => [a] -> [[a]]
pack [] = []
pack xs = [group xs] ++ pack (rest xs)
    where
        group [x] = [x]
        group (x:y:xs) = if x == y then x:(group (y:xs)) else [x]
        rest [x] = []
        rest (x:y:xs) = if x == y then rest (y:xs) else (y:xs)
