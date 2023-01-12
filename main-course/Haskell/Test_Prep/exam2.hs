-- task 1
-- findNb 1071225 --> 45
-- findNb 40539911473216 --> 3568
-- findNb 135440716410000 --> 4824
-- findNb 4183059834009 --> 2022
-- findNb 91716553919377 --> -1
-- findNb 24723578342962 --> -1

findNb :: Integer -> Integer
findNb m = find m 0 0
  where
    find m sum n
      | sum == m = n
      | sum > m = -1
      | otherwise = find m (sum + ((n + 1) ^ 3)) (n + 1)

-- task 2
-- dominates (+4) (*2) [1..4] --> True
-- dominates (+4) (*2) [1..5] --> False (5+4=9 < 5*2=10)

dominates :: (Int -> Int) -> (Int -> Int) -> [Int] -> Bool
dominates f g xs = and $ zipWith (>=) (generateRes f xs) (generateRes g xs)
  where
    generateRes func xs = [func x | x <- xs]

-- task 3
type Point = (Double, Double)
