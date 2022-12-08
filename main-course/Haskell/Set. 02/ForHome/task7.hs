import Data.Char (toLower)
import Data.List (group, nub, sort)

main :: IO ()
main = do
  print $ duplicateCount "" == 0 -- no characters repeats more than once
  print $ duplicateCount "abcde" == 0
  print $ duplicateCount "aabbcde" == 2 -- 'a' and 'b'
  print $ duplicateCount "aabBcde" == 2 -- 'a' occurs twice and 'b' twice (`b` and `B`)
  print $ duplicateCount "indivisibility" == 1 -- 'i' occurs six times
  print $ duplicateCount "Indivisibility" == 1
  print $ duplicateCount "aA11" == 2 -- 'a' and '1'
  print $ duplicateCount "ABBA" == 2 -- 'A' and 'B' each occur twice
  print $ duplicateCount "Indivisibilities" == 2 -- 'i' occurs seven times and 's' occurs twice
  print $ duplicateCount ['a' .. 'z'] == 0
  print $ duplicateCount (['a' .. 'z'] ++ ['A' .. 'Z']) == 26

duplicateCount :: String -> Int
duplicateCount "" = 0
duplicateCount text = sum $ lst $ group $ sort $ map toLower text
  where
    lst :: [String] -> [Int]
    lst xs = [1 | x <- xs, length x > 1]