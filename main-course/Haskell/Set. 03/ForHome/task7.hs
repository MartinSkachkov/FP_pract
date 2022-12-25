import Data.List (group, nub, sort)

main :: IO ()
main = do
  print $ isSorted [-5, -5, -6] == True
  print $ isSorted [-5, -5, -4] == True
  print $ isSorted [1, 1, 1, 1, 1, 1, 1, 1, 1] == True
  print $ isSorted [1, 2, 3, 3, 3, 4, 5, 6, 6] == True
  print $ isSorted [1, -1, -3, -3, -3, -4, -5, -6, -6] == True
  print $ isSorted [1, 2, 3, 3, 3, 4, 5, 6, 5] == False
  print $ isSorted [-100, -99, -99, -99] == True
  print $ isSorted [-100, -99, -99, -99, 100] == True
  print $ isSorted [100, 101, -102] == False
  print $ isSorted [1, 2, 3, 4, 5, 6] == True
  print $ isSorted [-1, -2, -3, -4, -5, -6] == True

  print $ isSortedXs [-5, -5, -6] == True
  print $ isSortedXs [-5, -5, -4] == True
  print $ isSortedXs [1, 1, 1, 1, 1, 1, 1, 1, 1] == True
  print $ isSortedXs [1, 2, 3, 3, 3, 4, 5, 6, 6] == True
  print $ isSortedXs [1, -1, -3, -3, -3, -4, -5, -6, -6] == True
  print $ isSortedXs [1, 2, 3, 3, 3, 4, 5, 6, 5] == False
  print $ isSortedXs [-100, -99, -99, -99] == True
  print $ isSortedXs [-100, -99, -99, -99, 100] == True
  print $ isSortedXs [100, 101, -102] == False
  print $ isSortedXs [1, 2, 3, 4, 5, 6] == True
  print $ isSortedXs [-1, -2, -3, -4, -5, -6] == True

isSorted :: (Ord a) => [a] -> Bool
isSorted [] = True
isSorted [x] = True
isSorted (x : y : xs)
  | x == y = isSorted (y : xs)
  | x < y = checkAscending (y : xs)
  | otherwise = checkDescending (y : xs)

isSortedXs :: (Ord a) => [a] -> Bool
isSortedXs [] = True
isSortedXs [x] = True
isSortedXs xs
  | length lst == 1 = True
  | head lst < lst !! 1 = checkAscending $ drop 1 lst
  | otherwise = checkDescending $ drop 1 lst
  where
    lst = concat $ map nub $ group xs

checkAscending :: (Ord a) => [a] -> Bool
checkAscending [] = True
checkAscending [x] = True
checkAscending (x : y : xs) = (x <= y) && checkAscending (y : xs)

checkDescending :: (Ord a) => [a] -> Bool
checkDescending [] = True
checkDescending [x] = True
checkDescending (x : y : xs) = (x >= y) && checkDescending (y : xs)