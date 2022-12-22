main :: IO()
main = do
    sample <- readFile "sample.txt"
    print (lines sample)
    print ((sum . (map (\ s -> read s :: Int)) . lines) sample)
