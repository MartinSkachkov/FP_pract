import Data.Char
import Data.List

main :: IO ()
main = do
  print $ numberBTree
  print $ charBTree

  print $ size numberBTree == 9
  print $ size charBTree == 7

  print $ sumTree numberBTree == 146

  print $ traverseDFS numberBTree == [96, 1, 12, 0, 5, 2, 4, 5, 21]
  print $ traverseDFS charBTree == "haskell"

  print $ getLevel numberBTree 2 == [1, 0, 2, 5]
  print $ getLevel charBTree 1 == "al"
  print $ getLevel charBTree 3 == []

  print $ traverseBFS numberBTree == [5, 12, 4, 1, 0, 2, 5, 96, 21]
  print $ traverseBFS charBTree == "kalhsel"

  print $ mapTree numberBTree (* 2) == Node 10 (Node 24 (Node 2 (Node 192 Empty Empty) Empty) (Node 0 Empty Empty)) (Node 8 (Node 4 Empty Empty) (Node 10 Empty (Node 42 Empty Empty)))
  print $ mapTree numberBTree (show) == Node "5" (Node "12" (Node "1" (Node "96" Empty Empty) Empty) (Node "0" Empty Empty)) (Node "4" (Node "2" Empty Empty) (Node "5" Empty (Node "21" Empty Empty)))
  print $ mapTree charBTree (toUpper) == Node 'K' (Node 'A' (Node 'H' Empty Empty) (Node 'S' Empty Empty)) (Node 'L' (Node 'E' Empty Empty) (Node 'L' Empty Empty))

data BTree a = Empty | Node a (BTree a) (BTree a)
  deriving (Show, Eq)

numberBTree :: BTree Int
numberBTree = Node 5 (Node 12 (Node 1 (Node 96 Empty Empty) Empty) (Node 0 Empty Empty)) (Node 4 (Node 2 Empty Empty) (Node 5 Empty (Node 21 Empty Empty)))

charBTree :: BTree Char
charBTree = Node 'k' (Node 'a' (Node 'h' Empty Empty) (Node 's' Empty Empty)) (Node 'l' (Node 'e' Empty Empty) (Node 'l' Empty Empty))

size :: BTree a -> Int
size Empty = 0
size (Node _ left right) = 1 + size left + size right

sumTree :: (Num a) => BTree a -> a
sumTree Empty = 0
sumTree (Node val left right) = val + sumTree left + sumTree right

traverseDFS :: BTree a -> [a]
traverseDFS Empty = []
traverseDFS (Node val left right) = traverseDFS left ++ [val] ++ traverseDFS right

getLevel :: BTree a -> Int -> [a]
getLevel Empty _ = []
getLevel (Node val _ _) 0 = [val]
getLevel (Node val left right) level = getLevel left (level - 1) ++ getLevel right (level - 1)

traverseBFS :: BTree a -> [a]
traverseBFS t = concat $ takeWhile (not . null) $ map (getLevel t) [0 ..]

mapTree :: BTree a -> (a -> b) -> BTree b
mapTree Empty _ = Empty
mapTree (Node val left right) f = Node (f val) (mapTree left f) (mapTree right f)

height :: BTree a -> Int
height Empty = 0
height (Node _ left right) = 1 + max (height left) (height right)

countLeaves :: BTree a -> Int
countLeaves Empty = 0
countLeaves (Node _ Empty Empty) = 1
countLeaves (Node _ left right) = countLeaves left + countLeaves right

sumLeaves :: (Num a) => BTree a -> a
sumLeaves Empty = 0
sumLeaves (Node val Empty Empty) = val
sumLeaves (Node _ left right) = sumLeaves left + sumLeaves right

averageTree :: BTree Double -> Double
averageTree Empty = 0
averageTree bt = sumTree bt / fromIntegral (size bt)

treeEq :: (Eq a) => BTree a -> BTree a -> Bool
treeEq Empty Empty = True
treeEq (Node val1 l1 r1) (Node val2 l2 r2) = val1 == val2 && treeEq l1 l2 && treeEq r1 r2
treeEq _ _ = False

maxDepth :: BTree a -> Int
maxDepth Empty = 0
maxDepth (Node val left right) = 1 + max (maxDepth left) (maxDepth right)

mirror :: BTree a -> BTree a
mirror Empty = Empty
mirror (Node val l r) = Node val (mirror r) (mirror l)