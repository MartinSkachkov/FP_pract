import Data.Char (toLower)
import Data.List (group, nub, sort, sortOn)

-- task 1
-- reverseOrdSuff 37563 → 36
-- reverseOrdSuff 32763 → 367
-- reverseOrdSuff 32567 → 7
-- reverseOrdSuff 32666 → 6

reverseOrdSuff :: Int -> Int
reverseOrdSuff x = read $ nub $ calculate strNum
  where
    calculate :: String -> String
    calculate [x] = [x]
    calculate (x : y : str)
      | x < y = [x] ++ [y] ++ calculate (y : str)
      | otherwise = [x]
    strNum = reverse $ show x

-- task 2
-- sumUnique [[1,2,3,2],[-4,-4],[5]] → 9 (= 1+3+5)
-- sumUnique [[2,2,2],[3,3,3],[4,4,4]] → 0
-- sumUnique [[1,2,3],[4,5,6],[7,8,9]] → 45

sumUnique :: [[Int]] -> Int
sumUnique [] = 0
sumUnique xss = sum $ concat $ calculate xss
  where
    calculate :: [[Int]] -> [[Int]]
    calculate [] = []
    calculate (xs : xss) = [ys | ys <- yss, length ys == 1] ++ calculate xss
      where
        yss = group $ sort xs

-- task 3
type Product = (String, Double)

type StoreAvailability = [Product]

store1 :: StoreAvailability
store1 = [("bread", 1), ("milk", 2.5), ("lamb", 10), ("cheese", 5), ("butter", 2.3)]

store2 :: StoreAvailability
store2 = [("bread", 1), ("cheese", 2.5), ("bread", 1), ("cheese", 5), ("butter", 2.3)]

-- closestToAverage store1
closestToAverage :: StoreAvailability -> String
closestToAverage [] = "Empty storage!"
closestToAverage products = fst $ head $ sortOn snd [(name, abs ((averagePrice products) - price)) | (name, price) <- products]
  where
    averagePrice :: StoreAvailability -> Double
    averagePrice [] = 0
    averagePrice products = sum pricesList / fromIntegral (length pricesList)
      where
        pricesList = [price | (name, price) <- products]

-- cheaperAlternative store2
cheaperAlternative :: StoreAvailability -> Int
cheaperAlternative [] = 0
cheaperAlternative products = sum [if oPrice < pPrice then 1 else 0 | (pName, pPrice) <- products, (oName, oPrice) <- products, pName == oName]

-- task 4
points :: [(Double, Double, Double)]
points = [(1, 2, 3), (4, 5, 6), (7, 8, 9)]

minDistance :: [(Double, Double, Double)] -> Double
minDistance [] = error "Can't calculate any distances!"
minDistance ps = minimum $ calcDistance ps
  where
    calcDistance [] = []
    calcDistance [p] = []
    calcDistance (p : ps) = [distance p currP | currP <- ps] ++ calcDistance ps
    distance (x1, y1, z1) (x2, y2, z2) = (x1 - x2) * (x1 - x2) + (y1 - y2) * (y1 - y2) + (z1 - z2) * (z1 - z2)

-- taks 5
-- reduceStr "dabAcCaCBAcCcaDD"

reduceStr :: String -> String
reduceStr s = foldr (\letter acc -> if acc /= "" && nextTo letter (head acc) then tail acc else letter : acc) "" s
  where
    nextTo c1 c2 = c1 /= c2 && (toLower c1 == toLower c2)

-- task 6
fn :: Double -> Double
fn = maximize [(\x -> x * x * x), (\x -> x + 1)]

maximize :: (Ord a, Num a, Fractional a) => [(a -> a)] -> (a -> a)
maximize [] = error "Empty list!"
maximize fs = \x -> maximum $ concat [map f [x] | f <- fs]

-- task 7
-- inverseFun (\x -> x+1) (\x -> x-1) 5 10 → True
-- inverseFun (\x -> x*x) (\x -> x^3) 0 1 → True
-- inverseFun (\x -> x+1) (\x -> x+2) 0 1 → False

inverseFun :: (Int -> Int) -> (Int -> Int) -> Int -> Int -> Bool
inverseFun f g start end = and [f (g x) == x && g (f x) == x | x <- [start .. end]]