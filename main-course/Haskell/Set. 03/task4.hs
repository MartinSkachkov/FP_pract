main :: IO ()
main = do
  print $ divideNoPM (10, 5) == (2, 0) -- 10 / 5 = 2 and 10 % 5 = 0
  print $ divideNoPM (69, 42) == (1, 27)

  print $ dividePM (10, 5) == (2, 0)
  print $ dividePM (69, 42) == (1, 27)

  print $ lambdaPM (10, 5) == (2, 0)
  print $ lambdaNoPM (69, 42) == (1, 27)

divideNoPM :: (Int, Int) -> (Int, Int)
divideNoPM t = (fst t `div` snd t, fst t `mod` snd t)

dividePM :: (Int, Int) -> (Int, Int)
dividePM (x, y) = (x `div` y, x `mod` y)

lambdaPM :: (Integer, Integer) -> (Integer, Integer)
lambdaPM = \(x, y) -> (x `div` y, x `mod` y)

lambdaNoPM :: (Integer, Integer) -> (Integer, Integer)
lambdaNoPM = \t -> (fst t `div` snd t, fst t `mod` snd t)