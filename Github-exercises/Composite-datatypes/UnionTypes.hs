import Data.Time (Day)

-- structure with multiple values together
data MarriageInfo = MarriageInfo
  { spouse :: String,
    date :: Day
  }
  deriving (Eq, Show)

data UserProfile = UserProfile
  -- fields
  { username :: String,
    age :: Int,
    active :: Bool,
    interests :: [String]
  }
  deriving (Eq, Show)

-- set of alternatives all valid but only one at a time
data RelationshipStatus
  = MarriedTo MarriageInfo
  | EngagedTo UserProfile
  | ItsComplicated
  | Single
  deriving (Eq, Show)

spouseName :: RelationshipStatus -> String
spouseName (MarriedTo MarriageInfo {spouse}) = spouse
spouseName (EngagedTo UserProfile {username}) = username
spouseName ItsComplicated = "no spouse"
spouseName Single = "no spouse"

user1 :: RelationshipStatus
user1 = EngagedTo UserProfile {username = "Marto", age = 20, active = True, interests = ["Sports", "Prog"]}

data HasSpouse
  = HasSpouse String
  | NoSpouse

spouseName' :: RelationshipStatus -> HasSpouse
spouseName' (MarriedTo MarriageInfo {spouse}) = HasSpouse spouse
spouseName' (EngagedTo UserProfile {username}) = HasSpouse username
spouseName' ItsComplicated = NoSpouse
spouseName' Single = NoSpouse

data DivisionResult
  = DivideSuccess Float
  | DivisionByZero
  deriving (Show)

divisionOrDefault :: Float -> DivisionResult -> Float
divisionOrDefault num DivisionByZero = num
divisionOrDefault num (DivideSuccess result) = result

divisionOrDefault' :: Float -> DivisionResult -> Float
divisionOrDefault' num divResult =
  case divResult of
    DivisionByZero -> num
    DivideSuccess result -> result

newtype TickerSymbol = TickerSymbol String
  deriving (Show)

data TradeOrder
  = SellOrder TickerSymbol Int
  | BuyOrder TickerSymbol Int
  deriving (Show)

correspondingOrderType :: TradeOrder -> TradeOrder
correspondingOrderType (BuyOrder (TickerSymbol str) num) = SellOrder (TickerSymbol str) num
correspondingOrderType (SellOrder (TickerSymbol str) num) = BuyOrder (TickerSymbol str) num

data MatchedOrNot
  = MatchedOrder [TradeOrder]
  | NoMathcedOrder

{-
matchOrder :: TradeOrder -> [TradeOrder] -> MatchedOrNot
matchOrder (SellOrder (TickerSymbol str) num) orders =
  if elem (correspondingOrderType (SellOrder (TickerSymbol str) num)) orders
    then MatchedOrder [correspondingOrderType (SellOrder (TickerSymbol str) num)]
    else NoMathcedOrder
-}