rotate :: [a] -> Int -> [a]
rotate xs 0 = xs
rotate (x:xs) n
              | n > 0 = rotate (xs ++ [x]) (n - 1)
              | n < 0 = rotate ([myLast (x:xs)] ++ myInit (x:xs)) (n + 1)
              where myLast [x] = x
                    myLast (x:xs) = myLast xs
                    myInit [x] = []
                    myInit (x:xs) = x:(myInit xs)
