import Data.Char (digitToInt)

main :: IO ()
main = do
  print $ sumSpecialPrimes 5 2 == 392
  print $ sumSpecialPrimes 5 3 == 107
  print $ sumSpecialPrimes 10 3 == 462

isPrime :: Int -> Bool
isPrime x = [z | z <- [1 .. x], mod x z == 0] == [1, x]

sumSpecialPrimes :: Int -> Int -> Int
sumSpecialPrimes n d = sum $ take n [x | x <- [2 ..], isPrime x, elem d (map digitToInt $ show x)]