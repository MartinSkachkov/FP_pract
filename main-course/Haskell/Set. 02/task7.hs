import Data.Char (toLower)
import Data.List (group, nub, sort)

main :: IO ()
main = do
  print $ countOccurrences "Test" == [('e', 1), ('s', 1), ('t', 2)]
  print $ countOccurrences "ThisIsAReallyLongWordContaingAlmostEveryCharacter" == [('a', 6), ('c', 3), ('d', 1), ('e', 4), ('g', 2), ('h', 2), ('i', 3), ('l', 4), ('m', 1), ('n', 3), ('o', 4), ('r', 5), ('s', 3), ('t', 4), ('v', 1), ('w', 1), ('y', 2)]

countOccurrences :: String -> [(Char, Int)]
-- countOccurrences text = map (\ys -> (head ys, length ys)) $ group $ sort $ map toLower text
countOccurrences = map (\ys -> (head ys, length ys)) . group . sort . map toLower