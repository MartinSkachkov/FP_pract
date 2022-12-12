main :: IO ()
main = do
  print $ toTuples [1, 2, 3, 4] [4, 3, 2, 1] == [(1, 4), (2, 3), (3, 2), (4, 1)]
  print $ toTuples [1.52, 2.75] [] == [(1.52, 0), (2.75, 0)]

toTuples :: (Num a, Enum a) => [a] -> [a] -> [(a, a)]
toTuples xs [] = zip xs [0, 0 ..]
toTuples [] ys = zip [0, 0 ..] ys
toTuples xs ys = zip xs ys

{-
toTuples :: (Num a, Enum a) => [a] -> [a] -> [(a, a)]
toTuples xs [] = zip xs $ replicate (length xs) 0
toTuples [] ys = zip (replicate (length ys) 0) ys
toTuples xs ys = zip xs ys
-}