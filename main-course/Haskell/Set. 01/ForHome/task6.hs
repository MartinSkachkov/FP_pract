main :: IO ()
main = do
  print $ isPrimeG 1 == False
  print $ isPrimeG 2 == True
  print $ isPrimeG 3 == True
  print $ isPrimeG 6 == False
  print $ isPrimeG 61 == True

  print $ isPrimeLC 1 == False
  print $ isPrimeLC 2 == True
  print $ isPrimeLC 3 == True
  print $ isPrimeLC 6 == False
  print $ isPrimeLC 61 == True

isPrimeLC :: Int -> Bool
isPrimeLC n = [x | x <- [1 .. n], n `mod` x == 0] == [1, n]

isPrimeG :: Int -> Bool
isPrimeG n
  | n <= 1 = False
  | otherwise = all ((/= 0) . (n `rem`)) [2 .. n - 1]