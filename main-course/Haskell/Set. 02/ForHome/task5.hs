import Data.Char (digitToInt, intToDigit)
import Data.List (nub)

main :: IO ()
main = do
  print $ reverseOrdSuff 37563 == 36
  print $ reverseOrdSuff 32763 == 367
  print $ reverseOrdSuff 32567 == 7
  print $ reverseOrdSuff 32666 == 6

reverseOrdSuff :: Int -> Int
reverseOrdSuff = joinInt . nub . maxSeq . map digitToInt . reverse . show
  where
    maxSeq :: [Int] -> [Int]
    maxSeq [] = []
    maxSeq [x] = [x]
    maxSeq xs 
     | xs !! 0 > xs !! 1 = [head xs]
    maxSeq [x, y] 
     | x < y = [x, y] 
     | otherwise = []
    maxSeq (x : y : xs)
     | x <= y = x : y : maxSeq (y : xs)
     | otherwise = []
    joinInt :: [Int] -> Int
    joinInt = read . map intToDigit

reverseOrdSuff' :: Int -> Int
reverseOrdSuff' = joinInt . nub . maxSeq . map digitToInt . reverse . show
 where
  maxSeq :: [Int] -> [Int]
  maxSeq [] = []
  maxSeq [x] = [x]
  maxSeq (x:y:xs)
   | x <= y =x: maxSeq (y:xs)
   | otherwise = [x]
  joinInt :: [Int] -> Int
  joinInt = read . map intToDigit