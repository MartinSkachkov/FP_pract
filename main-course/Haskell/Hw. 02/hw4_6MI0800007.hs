import Data.Char
import Data.List

main :: IO ()
main = do
  sample <- readFile "sample.txt"
  print $ decode sample == 61229

  input <- readFile "input.txt"
  print $ decode input == 983026

  print $ versionSum "8A004A801A8002F478" == 16
  print $ versionSum "620080001611562C8802118E34" == 9
  print $ versionSum "C0015000016115A2E0802F182340" == 23
  print $ versionSum "A0016C880162017C3686B18A3D4780" == 31

-- task 1
lenSixSignals :: Foldable t => [t a] -> [t a]
lenSixSignals signalList = [signal | signal <- signalList, length signal == 6]

lenFiveSignals :: Foldable t => [t a] -> [t a]
lenFiveSignals signalList = [signal | signal <- signalList, length signal == 5]

ninePermuts :: Eq a => [a] -> [a] -> [a] -> [[a]]
ninePermuts eight seven four = permutations ((head $ eight \\ nub (seven ++ four)) : nub (seven ++ four)) ++ permutations ((last $ eight \\ nub (seven ++ four)) : nub (seven ++ four))

sixPermuts :: Eq a => [a] -> [a] -> [[a]]
sixPermuts eight one = permutations (eight \\ init one) ++ permutations (eight \\ tail one)

fivePermuts :: Eq a => [a] -> [a] -> [[a]]
fivePermuts six nine = permutations $ six \\ (six \\ nine)

twoPermuts :: Eq a => [a] -> [a] -> [a] -> [a] -> [[a]]
twoPermuts eight five four one = permutations (nub $ (eight \\ five) ++ (eight \\ four) ++ (init $ four \\ one)) ++ permutations (nub $ (eight \\ five) ++ (eight \\ four) ++ (tail $ four \\ one))

-- signalList = ["acedgfb","cdfbe","gcdfa","fbcad","dab","cefabd","cdfgeb","eafb","cagedb","ab"]
tableOfNumbers :: (Ord a1, Num a2) => [[a1]] -> [a1] -> a2
tableOfNumbers signalList currSignal
  | sort currSignal == sort one = 1 -- "ab"
  | sort currSignal == sort seven = 7 -- "dab"
  | sort currSignal == sort four = 4 -- "eafb"
  | sort currSignal == sort eight = 8 -- "acedgfb"
  | sort currSignal == sort nine = 9 -- "cefabd"
  | sort currSignal == sort six = 6 -- "cdfgeb"
  | sort currSignal == sort five = 5 -- "cdfbe"
  | sort currSignal == sort two = 2 -- "gcdfa"
  | sort currSignal == sort three = 3 -- "fbcad"
  | sort currSignal == sort zero = 0 -- "cagedb"
  | otherwise = error "Invalid!"
  where
    one = concat [signal | signal <- signalList, length signal == 2] -- "ab"
    seven = concat [signal | signal <- signalList, length signal == 3] -- "dab"
    four = concat [signal | signal <- signalList, length signal == 4] -- "eafb"
    eight = concat [signal | signal <- signalList, length signal == 7] -- "acedgfb"
    nine = concat [signal | signal <- lenSixSignals signalList, permut <- ninePermuts eight seven four, signal == permut] -- "cefabd"
    six = concat [signal | signal <- lenSixSignals signalList, permut <- sixPermuts eight one, signal == permut] -- "cdfgeb"
    five = concat [signal | signal <- lenFiveSignals signalList, permut <- fivePermuts six nine, signal == permut] -- "cdfbe"
    two = concat [signal | signal <- lenFiveSignals signalList, permut <- twoPermuts eight five four one, signal == permut] -- "gcdfa"
    three = concat [signal | signal <- lenFiveSignals signalList, signal /= two, signal /= five] -- "fbcad"
    zero = concat [signal | signal <- lenSixSignals signalList, signal /= nine, signal /= six] -- "cagedb"

calculate :: Ord a => [[a]] -> [[a]] -> Int
calculate signalList resultSignal = read $ concat [show $ tableOfNumbers signalList res | res <- resultSignal]

recursion :: Ord a => [[[a]]] -> [[[a]]] -> [Int]
recursion [] [] = []
recursion (sx : signalList) (rx : resultList) = calculate sx rx : recursion signalList resultList

decode :: String -> Int
decode contents = sum $ recursion signalList resultList
  where
    splitInput = map words $ lines contents
    signalList = [take 10 lst | lst <- splitInput]
    resultList = [drop 11 lst | lst <- splitInput]

-- task 2
-- Converters
hexChar :: Num a => Char -> a
hexChar ch
  | ch == '0' = 0
  | ch == '1' = 1
  | ch == '2' = 2
  | ch == '3' = 3
  | ch == '4' = 4
  | ch == '5' = 5
  | ch == '6' = 6
  | ch == '7' = 7
  | ch == '8' = 8
  | ch == '9' = 9
  | ch == 'A' = 10
  | ch == 'B' = 11
  | ch == 'C' = 12
  | ch == 'D' = 13
  | ch == 'E' = 14
  | ch == 'F' = 15
  | otherwise = 0

hexToDec :: Num a => [Char] -> a
hexToDec [] = 0
hexToDec hexStr = hexChar (last hexStr) + 16 * hexToDec (init hexStr)

decToBin :: (Num a2, Integral a1) => a1 -> [a2]
decToBin 0 = [0]
decToBin n = reverse (helper n)
  where
    helper 0 = []
    helper n
      | n `mod` 2 == 1 = 1 : helper (n `div` 2)
      | n `mod` 2 == 0 = 0 : helper (n `div` 2)

binToDec :: String -> Int
binToDec [] = 0
binToDec binStr = process $ reverse binStr
 where
  process [] = 0
  process (num : binStr) = process binStr * 2 + digitToInt num

fixBinLength :: Num a => [a] -> [a]
fixBinLength binary
  | length binary `mod` 4 == 0 = binary
  | otherwise = recursion binary (length binary `mod` 4)
  where
    recursion _ 0 = binary
    recursion binary k = 0 : recursion binary (k - 1)

removeLiteral :: (Eq a, Num a) => [a] -> [a]
removeLiteral [] = []
removeLiteral bin
  | head bin == 1 = removeLiteral $ drop 5 bin
  | head bin == 0 = drop 5 bin

removeOperators :: (Eq a, Num a) => [a] -> [a]
removeOperators [] = []
removeOperators bin
  | head bin == 1 = drop 12 bin
  | head bin == 0 = drop 16 bin

-- 110100101111111000101000
-- VVVTTTAAAAABBBBBCCCCC__

versionSum :: String -> Int
versionSum "" = 0
versionSum hex = process $ fixBinLength $ decToBin $ hexToDec hex
  where
    process :: [Int] -> Int
    process [] = 0
    process bin
      | packType == 4 = version + process (removeLiteral $ drop 6 bin) -- remove the version & type and then start removing the literals
      | otherwise = version + process (removeOperators $ drop 6 bin)
        where
          version :: Int
          version = binToDec $ concatMap show $ take 3 bin
          packType :: Int
          packType = binToDec $ concatMap show $ take 3 $ drop 3 bin