data Cardinality a = Multiple Int a | Single a deriving (Show)

pack :: (Eq a) => [a] -> [[a]]
pack [] = []
pack xs = [group xs] ++ pack (rest xs)
    where
        group [x] = [x]
        group (x:y:xs) = if x == y then x:(group (y:xs)) else [x]
        rest [x] = []
        rest (x:y:xs) = if x == y then rest (y:xs) else (y:xs)

encodeModified :: (Eq a) => [a] -> [Cardinality a]
encodeModified xs = ((map list2length) . pack) xs
    where
        list2length [x] = Single x
        list2length (x:xs) = Multiple (length (x:xs)) x
