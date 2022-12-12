main :: IO ()
main = do
  print $ sumVectors (1, 2, 3) (4, 5, 6) == (5, 7, 9)
  print $ sumVectors (0, 0, 0) (100, 200, -300) == (100, 200, -300)

  print $ scaleVector (1, 2, 3) 5 == (5, 10, 15)
  print $ scaleVector (5, 2, 159) (-2) == (-10, -4, -318)

  print $ dotProduct (1, 2, 3) (7, 4, 1) == 18
  print $ dotProduct (5, 2, 159) (0, -1, -2) == -320

type Vector = (Int, Int, Int) -- alias

sumVectors :: Vector -> Vector -> Vector
sumVectors (x, y, z) (q, p, m) = (x + q, y + p, z + m)

scaleVector :: Vector -> Int -> Vector
scaleVector (x, y, z) n = (x * n, y * n, z * n)

dotProduct :: Vector -> Vector -> Int
dotProduct (x, y, z) (q, p, m) = x * q + y * p + z * m
