main :: IO ()
main = do
  print $ isPalindrome 6 == True
  print $ isPalindrome 1010 == False
  print $ isPalindrome 505 == True
  print $ isPalindrome 123321 == True
  print $ isPalindrome 654 == False

isPalindrome :: Int -> Bool
isPalindrome n = reverse (digits' n) == digits' n

digits' :: Int -> [Int]
digits' 0 = []
digits' n = n `rem` 10 : digits' (n `div` 10)