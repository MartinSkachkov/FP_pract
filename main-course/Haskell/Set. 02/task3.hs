main :: IO ()
main = do
  print $ getPalindromesLC 132465 == 8
  print $ getPalindromesLC 654546 == 8
  print $ getPalindromesLC 100001 == 100012
  print $ getPalindromesLC 21612 == 21614
  print $ getPalindromesLC 26362 == 26364
  print $ getPalindromesHOF 132465 == 8
  print $ getPalindromesHOF 654546 == 8
  print $ getPalindromesHOF 100001 == 100012
  print $ getPalindromesHOF 21612 == 21614
  print $ getPalindromesHOF 26362 == 26364

isPalindrome :: Int -> Bool
isPalindrome n = (read $ reverse $ show n) == n

getPalindromesLC :: Int -> Int
getPalindromesLC n = minimum divisors + maximum divisors
  where
    divisors = [x | x <- [2 .. n], isPalindrome x && mod n x == 0]

getPalindromesHOF :: Int -> Int
getPalindromesHOF n = minimum divisors + maximum divisors
  where
    divisors = filter (\x -> isPalindrome x && mod n x == 0) [2 .. n]

getPalindromesLCLet :: Int -> Int
getPalindromesLCLet n =
  let paliDivs = [x | x <- [2 .. n], isPalindrome x && mod n x == 0]
   in minimum paliDivs + maximum paliDivs