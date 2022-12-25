main :: IO ()
main = do
  print $ subLists [1, 2, 3, 4, 5, 6, 7, 8, 9, 10] 2 == [[1, 2], [3, 4], [5, 6], [7, 8], [9, 10]]
  print $ subLists [1, 2, 3, 4, 5, 6, 7, 8, 9, 10] 4 == [[1, 2, 3, 4], [5, 6, 7, 8], [9, 10]]

subLists :: [a] -> Int -> [[a]]
subLists [] _ = []
subLists xs k
  | k > 0 = take k xs : subLists (drop k xs) k
  | otherwise = error "k must be a natural number"