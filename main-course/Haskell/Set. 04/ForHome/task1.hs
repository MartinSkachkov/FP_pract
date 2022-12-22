main :: IO()
main = do
    print $ squareDigits 9119 == 811181
    print $ squareDigits (-9119) == (-811181)
    print $ squareDigits 5050 == 250250
    print $ 0 == 0

squareDigits :: Int -> Int
squareDigits 0 = 0
squareDigits n 
 | n > 0 = read $ concat $ convertToLst n
 | n < 0 = read $ concat $ "-" : convertToLst (abs n)
 where
  convertToLst :: Int -> [String]
  convertToLst 0 = []
  convertToLst n = convertToLst (n `div` 10) ++ [show $ (n `mod` 10) ^2]