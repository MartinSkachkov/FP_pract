data BTree = Empty | Node Int BTree BTree

t :: BTree
t = Node 6 (Node 3 Empty (Node 2 Empty (Node 1 Empty Empty))) (Node 5 (Node 0 Empty Empty) Empty)

constructMaxBTree :: [Int] -> BTree
constructMaxBTree [] = Empty
constructMaxBTree xs = Node maxEl (constructMaxBTree leftXs) (constructMaxBTree rightXs)
  where
    maxEl = maximum xs
    leftXs = takeWhile (/= maxEl) xs
    rightXs = tail $ dropWhile (/= maxEl) xs