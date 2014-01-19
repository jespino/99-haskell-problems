data Cardinality a = Multiple Int a | Single a deriving (Show)

encodeDirect :: (Eq a) => [a] -> [Cardinality a]
encodeDirect [] = []
encodeDirect [x] = [Single x]
encodeDirect xs = auxEncode xs 1
        where
            auxEncode [] counter = []
            auxEncode [x] counter = [if counter == 1 then Single x else (Multiple counter x)]
            auxEncode (x:y:xs) counter = if x == y then auxEncode (y:xs) (counter + 1) else (auxEncode [x] counter) ++ (auxEncode (y:xs) 1)
