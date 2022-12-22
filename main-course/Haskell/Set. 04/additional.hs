import Data.Char (chr, ord, toLower)

isPrime :: Int -> Bool
isPrime n = [x | x <- [1 .. n], n `mod` x == 0] == [1, n]

decomposePrimes :: Int -> (Int, Int)
decomposePrimes n
  | even n = head [(x, n - x) | x <- [0 .. n], isPrime x && isPrime (n - x)]
  | otherwise = error "n is not even"

encode :: String -> String -> String
encode [] _ = []
encode (x : crypto) (y : word) = encodeChar x y : encode crypto word

encodeChar :: Char -> Char -> Char
encodeChar x y = chr $ (((ord (toLower x) - 97) + (ord (toLower y) - 97)) `mod` 26) + 97

compose :: [(Int -> Int)] -> (Int -> Int)
compose fx = foldr1 (.) fx