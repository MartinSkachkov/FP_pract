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
isPrimeLC n = [x | x <- [1 .. n], mod n x == 0] == [1, n]

isPrimeG :: Integer -> Bool
isPrimeG n = not $ anyDividesInRange 2 $ pred n
  where
    anyDividesInRange :: Integer -> Integer -> Bool
    anyDividesInRange start end
      | start > end = False
      | otherwise = mod n start == 0 || anyDividesInRange (succ start) end