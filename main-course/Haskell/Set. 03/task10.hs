main :: IO ()
main = do
  print $ checkRootLC [(5, 25), (6, 36), (7, 47), (8, 88)] == [True, True, False, False]
  print $ checkRootHOF [(5, 25), (6, 36), (7, 47), (8, 88)] == [True, True, False, False]

checkRootLC :: [(Int, Int)] -> [Bool]
checkRootLC xss = [snd xs == (fst xs * fst xs) | xs <- xss]

checkRootHOF :: [(Int, Int)] -> [Bool]
checkRootHOF = map (\(x, y) -> y == x * x)