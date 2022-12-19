main :: IO ()
main = do
  print $ TwoD 6 7
  print $ ThreeD 6 7 8.43
  print $ getPointsHOF (\x -> x * x) (2 +) [TwoD 2 2, TwoD 1 2, TwoD 3 7] == [TwoD 2 2, TwoD 3 7]
  print $ getPointsLC (\x -> x * x) (2 +) [TwoD 2 2, TwoD 1 2, TwoD 3 7] == [TwoD 2 2, TwoD 3 7]

data Point a = TwoD a a | ThreeD a a a
  deriving (Show, Eq)

getPointsHOF :: (Eq a) => (a -> a) -> (a -> a) -> [Point a] -> [Point a]
getPointsHOF _ _ [] = error "Empty list!"
getPointsHOF f g sx = filter (\(TwoD x y) -> f x == g y) sx

getPointsLC :: (Eq a) => (a -> a) -> (a -> a) -> [Point a] -> [Point a]
getPointsLC _ _ [] = error "Empty list!"
getPointsLC f g sx = [p | p@(TwoD x y) <- sx, f x == g y]