main :: IO ()
main = do
  contents <- readFile "sample.txt"
  print $ map words $ lines $ contents
  putStrLn contents

{-
Function:	lines
Type:	String -> [String]
Description:	creates an array of string from the original one, new line characters serving as separators

Function:	words
Type:	String -> [String]
Description:	creates an array of string from the original one, white space characters serving as separators
-}

-- TO COMPILE:
-- be in the same directory
-- ghc --make task0
-- .\task0.exe