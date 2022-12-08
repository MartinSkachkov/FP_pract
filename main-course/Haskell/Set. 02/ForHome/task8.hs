import Data.Char (toLower)
import Data.List (group, nub)

main :: IO ()
main = do
  print $ reduceStr "dabAcCaCBAcCcaDD" == "dabCBAcaDD" -- dabAcCaCBAcCcaDD -> dabAaCBAcCcaDD -> dabCBAcCcaDD -> dabCBAcaDD

-- не знам как да я реша

reduceStr :: String -> String
reduceStr = concatMap nub . group . map toLower