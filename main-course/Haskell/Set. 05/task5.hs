data BTree = Nil | Node (Int -> Int) BTree BTree

mapTree :: BTree -> Int -> [Int]
mapTree Nil _ = []
mapTree (Node f Nil Nil) x = [f x]
mapTree (Node f left right) x = mapTree left (f x) ++ mapTree right (f x)