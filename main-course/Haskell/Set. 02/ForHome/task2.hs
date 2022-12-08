import Data.Char (digitToInt)

main :: IO ()
main = do
  print $ sumSpecialPrimes 5 2 == 392
  print $ sumSpecialPrimes 5 3 == 107
  print $ sumSpecialPrimes 10 3 == 462

-- тук не разбирам защо на тестовете трябва да излязат такива големи резултати
-- доколкото схванах условието трябва да сумираме числата от 2 .. n, които са прости и съдържат d едновременно
sumSpecialPrimes :: Int -> Int -> Int
sumSpecialPrimes n d = sum [x | x <- [2 .. n], isPrime x, d `elem` (map digitToInt $ show x)]
  where
    isPrime :: Int -> Bool
    isPrime x = [z | z <- [1 .. x], mod x z == 0] == [1, x]