import Data.List

and' :: Bool -> Bool -> Bool
and' True True = True
and' _ _ = False

or' :: Bool -> Bool -> Bool
or' True _ = True
or' _ True = True
or' _ _ = False

table :: (Bool -> Bool -> Bool) -> String
table f = unlines [ intercalate " " [show x, show y, show $ f x y] | x <- [True,False], y <- [True, False]]
