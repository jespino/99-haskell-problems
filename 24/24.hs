import System.Random
import System.Locale
import Data.Time
import Data.Time.Format

diff_select :: Int -> Int -> IO [Int]
diff_select n m = do
                    epoch <- getZonedTime >>= time_to_epoch
                    return $ take n $ randomRs (0, m) (mkStdGen epoch)
              where time_to_epoch time = return $ read $ formatTime defaultTimeLocale "%s" time
