main :: IO ()
main = do
  print $ addOneXsPA [1, 2, 3, 4, 5] == [2, 3, 4, 5, 6]
  print $ addOneNPA 5 == 6
  print $ sqPlusOne 5 == 26

addOneXsPA :: (Num a) => [a] -> [a]
-- addOneXsPA xs = map (+ 1) xs
addOneXsPA = map (+ 1)

addOneNPA :: (Num a) => a -> a
-- addOneNPA n = n + 1
addOneNPA = (+ 1)

sqPlusOne :: (Num a) => a -> a
-- sqPlusOne n = n ^ 2 + 1
sqPlusOne = (+ 1) . (^ 2)