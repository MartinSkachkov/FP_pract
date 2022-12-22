main :: IO ()
main = do
  print $ matching "1234" == []
  print $ matching ",[.[-],]" == [(3, 5), (1, 7)]
  print $ matching ",+[-.,+]" == [(2, 7)]
  print $ matching "[][]" == [(0, 1), (2, 3)]

data Stack a = Stk [a]
  deriving (Show)

-- Stack template
push :: a -> Stack a -> Stack a
push x (Stk xs) = Stk (x : xs)

pop :: Stack a -> Stack a
pop (Stk (_ : xs)) = Stk xs
pop _ = error "Stack.pop: empty stack"

top :: Stack a -> a
top (Stk (x : _)) = x
top _ = error "Stack.top: empty stack"

empty :: Stack a
empty = Stk []

isEmpty :: Stack a -> Bool
isEmpty (Stk []) = True
isEmpty (Stk _) = False

-------------------------------------------

matching :: String -> [(Int, Int)]
matching str
  | not $ '[' `elem` str && ']' `elem` str = []
  | otherwise = recursion str (Stk []) 0
  where
    recursion [] _ _ = []
    recursion (l : str) (Stk xs) c
      | l == '[' = recursion str (push c (Stk xs)) (c + 1)
      | l == ']' = (top (Stk xs), c) : recursion str (pop (Stk xs)) (c + 1)
      | otherwise = recursion str (Stk xs) (c + 1)