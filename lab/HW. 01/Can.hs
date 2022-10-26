{-# LANGUAGE EmptyDataDeriving #-}

module Can where

import Binary (Binary (..), Bit (..))

data LeadingOne
  = LeadOne -- represents a the binary number 1
  | LeadingOne ::. Bit -- adding a bit at the end of a leading 1 binary number
  deriving (Show)

canOne :: LeadingOne
canOne = LeadOne -- 1

-- canonical number
data Can
  = LeadZero -- represents a binary number 0
  | Binary LeadingOne -- represents a binary number which always starts with a leading 1
  deriving (Show)

canZero :: Can
canZero = LeadZero -- 0

snoc :: Can -> Bit -> Can
snoc LeadZero Zero = LeadZero -- 00 -> 0
snoc LeadZero One = Binary LeadOne -- 01 -> 1
snoc (Binary x) y = Binary (x ::. y) -- concatenates a bit at the end
-- snoc (Binary ((LeadOne ::. One) ::. One)) Zero -> Binary (((LeadOne ::. One) ::. One) ::. Zero)

forgetLeadingOne :: LeadingOne -> Binary
forgetLeadingOne LeadOne = End :. One
forgetLeadingOne (x ::. Zero) = forgetLeadingOne x :. Zero
forgetLeadingOne (x ::. One) = forgetLeadingOne x :. One

-- ghci> forgetLeadingOne (((LeadOne ::. Zero ) ::. Zero) ::. One)
-- (((End :. One) :. Zero) :. Zero) :. One

forget :: Can -> Binary
forget = undefined

canonicalise :: Binary -> Can
canonicalise = undefined