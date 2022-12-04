main :: IO ()
main = do
  print $ hasIncDigits 1244 == True
  print $ hasIncDigits 12443 == False

hasIncDigits :: Int -> Bool
hasIncDigits n = process $ show n
  where
    process :: String -> Bool
    process [] = True
    process [x] = True
    process (x : y : xs) = x <= y && process (y : xs)