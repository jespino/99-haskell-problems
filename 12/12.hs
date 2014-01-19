data Cardinality a = Multiple Int a | Single a deriving (Show)

decodeModified :: (Eq a) => [Cardinality a] -> [a]
decodeModified [] = []
decodeModified ((Multiple x y):xs) = (replicate x y) ++ (decodeModified xs)
decodeModified ((Single y):xs) = y:(decodeModified xs)
