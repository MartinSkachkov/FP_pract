import Data.Char
import Data.List (group, sort)

toDigits :: Int -> [Int]
toDigits 0 = []
toDigits n = toDigits (n `div` 10) ++ [n `mod` 10]

isNarcisstic :: Int -> Bool
isNarcisstic n = sum [x ^ (length $ toDigits n) | x <- toDigits n] == n

mergeList :: [Int] -> [Int] -> [Int]
mergeList xs ys = sort $ xs ++ ys

mergeListPm :: [Int] -> [Int] -> [Int]
mergeListPm xs [] = xs
mergeListPm [] ys = ys
mergeListPm (x : xs) (y : ys)
  | x <= y = x : mergeListPm xs (y : ys)
  | otherwise = y : mergeListPm (x : xs) ys

mergeSort :: [Int] -> [Int]
mergeSort [] = []
mergeSort [x] = [x]
mergeSort xs = mergeListPm (mergeSort firstHalf) (mergeSort secondHalf)
  where
    mid = (length xs) `div` 2
    firstHalf = take mid xs
    secondHalf = drop mid xs

matchingChars :: Char -> Char -> Bool
matchingChars c1 c2 = c1 /= c2 && toLower c1 == toLower c2

foldPolymer :: String -> String
foldPolymer str = foldr (\letter acc -> if letter /= ""  && matchingChars (head acc) letter then tail acc else letter : acc) [] str