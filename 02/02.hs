myButLast :: [a] -> a
myButLast [x,y] = x
myButLast (x:xs) = myButLast xs
