main :: IO ()
main = do
  print $ combine [(1, 2), (1, 2)] == (11, 22)
  print $ combine [(3, 9), (8, 7), (7, 9), (8, 8), (5, 0), (9, 2)] == (377802, 989859)

combine :: [(Int, Int)] -> (Int, Int)
combine = foldl (\(x1, y1) (x2, y2) -> (x1 * 10 + min x2 y2, y1 * 10 + max x2 y2)) (0, 0)

{-
combine :: [(Int, Int)] -> (Int, Int)
combine [] = error "Empty list!"
combine xs = (smaller xs, larger xs)
  where
    smaller :: [(Int, Int)] -> Int
    smaller xs = read $ concat [show $ min x y | (x, y) <- xs]
    larger :: [(Int, Int)] -> Int
    larger xs = read $ concat [show $ max x y | (x, y) <- xs]
-}