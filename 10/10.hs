pack :: (Eq a) => [a] -> [[a]]
pack [] = []
pack xs = [group xs] ++ pack (rest xs)
    where
        group [x] = [x]
        group (x:y:xs) = if x == y then x:(group (y:xs)) else [x]
        rest [x] = []
        rest (x:y:xs) = if x == y then rest (y:xs) else (y:xs)

encode :: (Eq a) => [a] -> [(Int, a)]
encode xs = ((map list2length) . pack) xs
    where
        list2length (x:xs) = (length (x:xs), x)
