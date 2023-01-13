import Data.Char
import Data.List

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
-- splitPoints (1,1) 5 [(1,2),(2,3),(10,15),(-1,1),(12,14)]

type Point = (Double, Double)

splitPoints :: Point -> Double -> [Point] -> ([Point], [Point])
splitPoints c r ps = ([p | p <- ps, isInsideCircle p c], [p | p <- ps, not $ isInsideCircle p c])
  where
    isInsideCircle (x, y) (cX, cY)
      | sqrt ((x - cX) ^ 2 + (y - cY) ^ 2) < r = True
      | otherwise = False

-- task 4
data BTree = Empty | Node Int BTree BTree

t1 :: BTree
t1 = Node 8 (Node 3 (Node 1 Empty Empty) (Node 4 Empty Empty)) (Node 10 (Node 9 Empty Empty) (Node 14 Empty Empty))

t2 :: BTree
t2 = Node 8 (Node 3 (Node 1 Empty Empty) (Node 4 Empty Empty)) (Node 10 (Node 5 Empty Empty) (Node 14 Empty Empty))

t3 :: BTree
t3 = Node 8 (Node 3 (Node 5 Empty Empty) (Node 6 Empty Empty)) (Node 10 (Node 9 Empty Empty) (Node 14 Empty Empty))

traverseDFS :: BTree -> [Int]
traverseDFS Empty = []
traverseDFS (Node val l r) = traverseDFS l ++ [val] ++ traverseDFS r

isBST :: BTree -> Bool
isBST t = elements == sort elements
  where
    elements = traverseDFS t