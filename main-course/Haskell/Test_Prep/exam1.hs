import Data.Char
import Data.List

-- task 1
-- squareDigits 9119 ➝ 811181
-- squareDigits (-9119) ➝ -811181

squareDigits :: Int -> Int
squareDigits 0 = 0
squareDigits n
  | n > 0 = read $ concat $ process $ map digitToInt $ show $ abs n
  | otherwise = read $ concat $ "-" : (process $ map digitToInt $ show $ abs n)
  where
    process :: [Int] -> [String]
    process ds = [show (d ^ 2) | d <- ds]

-- task 2
-- stocklist stocks ['A','B'] ➝ [('A',200),('B',1140)]
-- stocklist stocks ['C','X'] ➝ [('C',500),('X',0)]
-- stocklist stocks ['Y','X'] ➝ [('Y',0),('X',0)]
-- stocklist stocks ['C'] ➝ [('C', 500)]

data Stock = Stock String Int

stocks :: [Stock]
stocks = [Stock "ABAR" 200, Stock "CDXE" 500, Stock "BKWR" 250, Stock "BTSQ" 890, Stock "DRTY" 600]

stocklist :: [Stock] -> String -> [(Char, Int)]
stocklist [] _ = error "Empty stock!"
stocklist _ [] = []
stocklist sx (c : cx) = calculate c ++ stocklist sx cx
  where
    calculate c = [(c, sum [quant | (Stock label quant) <- sx, head label == c])]

-- task 3
-- matching "1234" ➝ []
-- matching ",[.[-],]" ➝ [(3,5),(1,7)]
-- matching ",+[-.,+]" ➝ [(2,7)]
-- matching "[][]" ➝ [(0,1),(2,3)]

matching :: String -> [(Int, Int)]
matching "" = []
matching str = helper [] (zip str [0 ..])
  where
    helper _ [] = []
    helper idxs (('[', idxOp) : xs) = helper (idxOp : idxs) xs
    helper (i : idxs) ((']', idxCl) : xs) = (i, idxCl) : helper idxs xs
    helper idxs (_ : xs) = helper idxs xs

-- taks 4
-- isPerfectlyBalanced t1 == True

data BTree a = Nil | Node a (BTree a) (BTree a)

t1 :: BTree Char
t1 = Node 'H' (Node 'a' (Node 'k' Nil Nil) (Node 'e' Nil Nil)) (Node 's' (Node 'l' Nil Nil) (Node 'l' Nil Nil))

countNodes :: BTree a -> Int
countNodes Nil = 0
countNodes (Node _ left right) = 1 + countNodes left + countNodes right

height :: BTree a -> Int
height Nil = 0
height (Node _ left right) = 1 + max (height left) (height right)

isPerfectlyBalanced :: BTree a -> Bool
isPerfectlyBalanced Nil = True
isPerfectlyBalanced t = countNodes t == (2 ^ height t - 1)
