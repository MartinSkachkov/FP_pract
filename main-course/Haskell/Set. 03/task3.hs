main :: IO ()
main = do
  print $ sumTupleNoPM (4, 5) == 9
  print $ sumTupleNoPM (-5, 5) == 0

  print $ sumTuplePM (4, 5) == 9
  print $ sumTuplePM (-5, 5) == 0

  print $ lambdaPM (4, 5) == 9
  print $ lambdaNoPM (-5, 5) == 0

sumTupleNoPM :: (Int, Int) -> Int
sumTupleNoPM t = fst t + snd t

sumTuplePM :: (Int, Int) -> Int
sumTuplePM (x, y) = x + y

lambdaPM :: (Integer, Integer) -> Integer
lambdaPM = \(x, y) -> x + y

lambdaNoPM :: (Integer, Integer) -> Integer
lambdaNoPM = \t -> fst t + snd t
