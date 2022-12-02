main :: IO ()
main = do
  print $ hasIncDigits 1244 == True
  print $ hasIncDigits 12443 == False

hasIncDigits :: Int -> Bool
hasIncDigits n = process $ convertToList n
  where
    process :: [Int] -> Bool
    process [] = True
    process [x] = True
    process (x : y : xs) = x <= y && process (y : xs)

digits' :: Int -> [Int]
digits' 0 = []
digits' n = n `rem` 10 : digits' (n `div` 10)

convertToList :: Int -> [Int]
convertToList n = reverse $ digits' n