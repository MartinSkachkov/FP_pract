main :: IO ()
main = do
  print $ primesInRange 1 100 == [2, 3, 5, 7, 11, 13, 17, 19, 23, 29, 31, 37, 41, 43, 47, 53, 59, 61, 67, 71, 73, 79, 83, 89, 97]
  print $ primesInRange 100 1 == [2, 3, 5, 7, 11, 13, 17, 19, 23, 29, 31, 37, 41, 43, 47, 53, 59, 61, 67, 71, 73, 79, 83, 89, 97]

primesInRange :: Int -> Int -> [Int]
primesInRange x y = [z | z <- [min x y .. max x y], isPrime z]
  where
    isPrime :: Int -> Bool
    isPrime n = [x | x <- [1 .. n], mod n x == 0] == [1, n]