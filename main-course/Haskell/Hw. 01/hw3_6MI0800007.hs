import Data.List (nub)

main :: IO ()
main = do
  -- tests
  print $ generate [10, 15, 25] [1, 5, 20, 30] == [[10, 20], [10, 30], [15, 20], [15, 30], [25, 30], [10, 20, 25, 30], [15, 20, 25, 30]]
  print $ generate [1, 5, 8] [2, 6, 67] == [[1, 2], [1, 6], [1, 67], [5, 6], [5, 67], [8, 67], [1, 2, 5, 6], [1, 2, 5, 67], [1, 2, 8, 67], [1, 6, 8, 67], [5, 6, 8, 67], [1, 2, 5, 6, 8, 67]]
  print $ generate [1] [] == []
  print $ generate [1] [0] == []
  print $ generate [] [1, 3, 4] == []
  print $ generate [1] [1, 2, 3] == [[1, 2], [1, 3]]
  print $ generate [1, 3] [2] == [[1, 2]]
  print $ generate [1] [2] == [[1, 2]]

  input <- readFile "input.txt"
  print $ countUniques input == 245
  sample <- readFile "sample.txt"
  print $ countUniques sample == 26

-- task 1
construct :: [[Int]] -> [(Int, Int)] -> [[Int]]
construct xss ts = [xs ++ [a, b] | xs <- xss, (a, b) <- ts, last xs < a]

generate :: [Int] -> [Int] -> [[Int]]
generate xs ys = nub $ recursion twos
  where
    tuples = [(x, y) | x <- xs, y <- ys, x < y]
    twos = [[x, y] | x <- xs, y <- ys, x < y]
    recursion :: [[Int]] -> [[Int]]
    recursion list
      | null rest = list
      | otherwise = list ++ rest ++ recursion rest
      where
        rest = construct list tuples

-- task 2
countUniques :: String -> Int
countUniques contents = sum $ map sum [[1 | val <- vals, length val == 2 || length val == 3 || length val == 4 || length val == 7] | vals <- outputVals]
  where
    splitInput = map words $ lines contents
    outputVals = [drop 11 c | c <- splitInput]