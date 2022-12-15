import Data.List (nub)

main :: IO ()
main = do
  print $ isImage [1, 2, 3, 4] [2, 3, 4, 5] == (True, 1)
  print $ isImage [1, 2, 3, 4] [4, 5, 6, 7] == (True, 3)
  print $ isImage [4, 5, 6, 7] [1, 2, 3, 4] == (True, -3)
  print $ isImage [1, 2, 3, 4] [4, 5, 6, 6] == (False, 0)
  print $ isImage [1, 2] [-1, -2] == (False, 0)
  print $ isImage [1, 2, 3, 4] [2, 3, 4, 4] == (False, 0)

isImage :: [Int] -> [Int] -> (Bool, Int)
isImage [] [] = (True, 0)
isImage xs ys = (predicate, if predicate then read $ concat $ map show genLst else 0)
  where
    genLst = nub $ zipWith (-) ys xs
    predicate = 1 == length genLst