main :: IO ()
main = do
  print $ stockList stocks ['A', 'B'] == [('A', 200), ('B', 1140)]
  print $ stockList stocks ['C', 'X'] == [('C', 500), ('X', 0)]
  print $ stockList stocks ['Y', 'X'] == [('Y', 0), ('X', 0)]
  print $ stockList stocks ['C'] == [('C', 500)]

data Stock = Stock String Int
  deriving (Show)

stocks :: [Stock]
stocks = [Stock "ABAR" 200, Stock "CDXE" 500, Stock "BKWR" 250, Stock "BTSQ" 890, Stock "DRTY" 600]

stockList :: [Stock] -> String -> [(Char, Int)]
stockList stockLst categories = map (\category -> (category, getAmount category validStock)) categories
  where
    validStock = filter (\(code, quant) -> elem code categories) $ map (\(Stock (code : rest) quant) -> (code, quant)) stockLst
    getAmount :: Char -> [(Char, Int)] -> Int
    getAmount _ [] = 0
    getAmount ch ((code, amount) : leftovers)
      | ch == code = amount + getAmount ch leftovers
      | otherwise = getAmount ch leftovers