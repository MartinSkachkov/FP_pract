main :: IO ()
main = do
  print $ (rf ((-) (-4)) (* (-2))) [1 .. 10] (* 3) == [15, 18, 21, 24, 27, 30]

{-
1. What is the name of the procedure/function?
   rf
2. How many parameters does it take?
   4 parameters
3. What is the type of the parameters?
   (Int -> Int), (Int -> Int), [Int], (Int -> Int)
4. What are the names of the parameters?
   function -> function -> list -> function
5. What is the return type?
   list ([Int])
-}

rf :: (Int -> Int) -> (Int -> Int) -> [Int] -> (Int -> Int) -> [Int]
rf _ _ [] _ = []
rf f g (x : xs) h
  | f x > g x = h x : rf f g xs h
  | otherwise = rf f g xs h