main :: IO ()
main = do
  print $ (rf ((-) (-4)) (* (-2))) [1 .. 10] (* 3) == [15, 18, 21, 24, 27, 30]

rf :: (Int -> Int) -> (Int -> Int) -> [Int] -> (Int -> Int) -> [Int]
rf _ _ [] _ = []
rf f g (x : xs) h
  | f x > g x = h x : rf f g xs h
  | otherwise = rf f g xs h