main :: IO ()
main = do
  print $ getVolumes [(5, 10), (5, 2), (2, 10), (2, 5)] == [785.3981633974483, 157.07963267948966, 125.66370614359172, 62.83185307179586]

getVolumes :: (Num a, Fractional a) => [(a, a)] -> [a]
getVolumes [] = []
getVolumes xss = map (\(r, h) -> r ^ 2 * h * 3.14) xss

{-
getVolumes' :: [(Int, Int)] -> [Double]
getVolumes' [] = []
getVolumes' xss = map (\(r, h) -> fromIntegral r ^ 2 * fromIntegral h * 3.14) xss
-}