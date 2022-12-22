main :: IO ()
main = do
  print $ addN [1, 2, 3, 4, 5] 9999999999999999999999 == [10000000000000000000000, 10000000000000000000001, 10000000000000000000002, 10000000000000000000003, 10000000000000000000004]
  print $ sqAddN [1, 2, 3, 4, 5] 5 == [36, 49, 64, 81, 100]
  print $ divByN [1, 2, 3, 4, 5] 5 == [0.2, 0.4, 0.6, 0.8, 1.0]
  print $ divByN [1, 2, 3, 4, 5] (-5) == [-0.2, -0.4, -0.6, -0.8, -1.0]
  print $ divByN [1, 2, 3, 4, 5] 0
  print $ filterByN [1, 2, 3, 4, 5] 3 == [3, 4, 5]

addN :: (Num b) => [b] -> b -> [b]
addN xs n = map (+ n) xs

sqAddN :: (Num b) => [b] -> b -> [b]
sqAddN xs n = map ((^ 2) . (+ n)) xs

divByN :: (Fractional b, Integral a1, Integral a2) => [a2] -> a1 -> [b]
divByN _ 0 = error "Can't divide by zero!"
divByN xs n = map ((/ fromIntegral n) . fromIntegral) xs

filterByN :: Ord a => [a] -> a -> [a]
filterByN xs n = filter (>= n) xs