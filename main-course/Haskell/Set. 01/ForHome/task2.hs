main :: IO ()
main = do
  print $ myGcdG 5 13 == 1
  print $ myGcdG 13 1235 == 13

  print $ myGcdPM 5 13 == 1
  print $ myGcdPM 13 1235 == 13

myGcdG :: Int -> Int -> Int
myGcdG a b
  | b == 0 = abs a
  | otherwise = myGcdG b (a `mod` b)

myGcdPM :: Int -> Int -> Int
myGcdPM a 0 = a
myGcdPM a b = myGcdPM b (a `mod` b)