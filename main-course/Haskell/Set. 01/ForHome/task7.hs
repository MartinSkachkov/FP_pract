main :: IO ()
main = do
  print $ isPerfect 1 == False
  print $ isPerfect 6 == True
  print $ isPerfect 495 == False
  print $ isPerfect 33550336 == True

isPerfect :: Integer -> Bool
isPerfect n = n >= 0 && sum (divisors n) == n

divisors :: Integer -> [Integer]
divisors n = [x | x <- [1 .. n - 1], x `divides` n]
  where
    divides :: Integer -> Integer -> Bool
    divides x n = n `mod` x == 0