module Binary where

data Binary
  = End
  | Binary :. Bit
  deriving (Show)

data Bit = Zero | One
  deriving (Show)

infixl 6 :. -- ((End :. One) :. Zero) :. One

succBinary :: Binary -> Binary
succBinary End = End :. One -- 0 -> 1
succBinary (bin :. Zero) = bin :. One -- 10 -> 11
succBinary (bin :. One) = succBinary bin :. Zero -- 11 -> 100

-- succBinary (End :. One :. One)
-- succBinary (    bin    :. One)
-- ((succBinary End :. One) :. Zero)
-- (succBinary End) :. Zero)
-- (One)
-- One Zero Zero

integerToBinary :: Integer -> Binary
integerToBinary 0 = End
integerToBinary 1 = End :. One
integerToBinary num =
  if ((num `rem` 2) == 0)
    then integerToBinary (num `div` 2) :. Zero
    else integerToBinary (num `div` 2) :. One

binaryToInteger :: Binary -> Integer
binaryToInteger End = 0
binaryToInteger (bin :. Zero) = 2 * binaryToInteger bin
binaryToInteger (bin :. One) = 2 * binaryToInteger bin + 1

hasLeadingZero :: Binary -> Bool
hasLeadingZero End = False
hasLeadingZero (End :. Zero) = True
hasLeadingZero (bin :. _) = hasLeadingZero bin

isEnd :: Binary -> Bool
isEnd End = True
isEnd (bin :. Zero) = isEnd bin
isEnd _ = False

canonicalise :: Binary -> Binary
canonicalise End = End
canonicalise (bin :. One) = canonicalise bin :. One
canonicalise (bin :. Zero) =
  if (isEnd bin)
    then End
    else canonicalise bin :. Zero

addBinary :: Binary -> Binary -> Binary
addBinary End End = End
addBinary bin End = bin
addBinary End bin = bin
addBinary (bin1 :. Zero) (bin2 :. Zero) = addBinary bin1 bin2 :. Zero
addBinary (bin1 :. One) (bin2 :. Zero) = addBinary bin1 bin2 :. One
addBinary (bin1 :. Zero) (bin2 :. One) = addBinary bin1 bin2 :. One
addBinary (bin1 :. One) (bin2 :. One) = addBinary (bin1 :. One) bin2 :. Zero -- carry