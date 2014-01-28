import Data.Bits
import Numeric
import Data.Char

gray :: Int -> [String]
gray n = [ showBinary n $ x `xor` (shiftR x 1) | x <- [0..pow]]
    where pow :: Int
          pow = (2 ^ n) - 1
          showBinary l n = zeroFill l $ showIntAtBase 2 intToDigit n ""
          zeroFill :: Int -> String -> String
          zeroFill n xs
                     | length xs >= n = xs
                     | length xs < n = zeroFill n ('0':xs)
