group :: [Int] -> [a] -> [[a]]
group [] _ = []
group _ [] = []
group (n:ns) xs = take n xs:(group ns (drop n xs))
