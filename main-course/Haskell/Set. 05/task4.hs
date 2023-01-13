import Data.Char
import Data.List

main :: IO ()
main = do
  print $ isBst t3 == True
  print $ isBst t4 == False
  print $ isBst t5 == False
  print $ isBst t6 == False

data BTree a = Empty | Node a (BTree a) (BTree a)
  deriving (Show)

t3 :: (Num a) => BTree a
t3 = Node 8 (Node 3 (Node 1 Empty Empty) (Node 6 (Node 4 Empty Empty) (Node 7 Empty Empty))) (Node 10 Empty (Node 14 (Node 13 Empty Empty) Empty))

t4 :: (Num a) => BTree a
t4 = Node 8 (Node 3 (Node 5 Empty Empty) (Node 6 Empty Empty)) (Node 10 Empty (Node 14 Empty Empty))

t5 :: (Num a) => BTree a
t5 = Node 8 (Node 3 (Node 2 Empty Empty) (Node 6 Empty Empty)) (Node 10 Empty (Node 1 Empty Empty))

t6 :: (Num a) => BTree a
t6 = Node 8 (Node 3 (Node 1 Empty Empty) (Node 4 Empty Empty)) (Node 10 (Node 5 Empty Empty) Empty)

traverseDFS :: BTree a -> [a]
traverseDFS Empty = []
traverseDFS (Node val l r) = traverseDFS l ++ [val] ++ traverseDFS r

isBst :: (Eq a, Ord a) => BTree a -> Bool
isBst tree = sort nodes == nodes
  where
    nodes = traverseDFS tree