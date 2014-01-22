import System.Random
import System.Locale
import Data.Time
import Data.Time.Format

rnd_select :: [a] -> Int -> IO [a]
rnd_select xs n = do
                    epoch <- getZonedTime >>= time_to_epoch
                    return $ take n [xs !! x | x <- (random_list $ mkStdGen epoch)]
          where
              random_list generator = randomRs (0, (length xs) - 1) generator
              time_to_epoch time = return $ read $ formatTime defaultTimeLocale "%s" time
