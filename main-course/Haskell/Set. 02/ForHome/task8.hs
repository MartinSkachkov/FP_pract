import Data.Char (toLower)
import Data.List (group, nub)

main :: IO ()
main = do
  print $ reduceStr "dabAcCaCBAcCcaDD" == "dabCBAcaDD" -- dabAcCaCBAcCcaDD -> dabAaCBAcCcaDD -> dabCBAcCcaDD -> dabCBAcaDD

reduceStr :: String -> String
reduceStr s =
  foldr
    ( \letter acc ->
        if acc /= "" && nextTo (head acc) letter
          then tail acc
          else letter : acc
    )
    []
    s
  where
    nextTo :: Char -> Char -> Bool
    nextTo c1 c2 = c1 /= c2 && toLower c1 == toLower c2