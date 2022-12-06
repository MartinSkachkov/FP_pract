import Data.Char (digitToInt)
import Data.List (sort)

main :: IO ()
main = do
  print $ isAscending 0 == True
  print $ isAscending 10 == False
  print $ isAscending 122 == True
  print $ isAscending 123 == True
  print $ isAscending 12340 == False
  print $ isAscending 12349 == True

isAscending :: Int -> Bool
isAscending n = xs == sort xs
  where
    xs = map digitToInt $ show n -- this will convert n to a list

isAscending2 :: Int -> Bool
isAscending2 n = and $ zipWith (<=) nXs (tail nXs)
  where
    -- isAscending2 n = all (\ (x, y) -> x <= y) $ zip nXs (tail nXs)
    nXs = map digitToInt $ show n