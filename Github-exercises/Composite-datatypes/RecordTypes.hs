-- store multiple values together in a named structure
-- individual parts, or "fields", are named as well
-- the constructor name can be different than the type name, but this is comparatively rare.

--      type       constructor
data UserProfile = UserProfile
  -- fields
  { username :: String,
    age :: Int,
    active :: Bool,
    interests :: [String]
  }
  deriving (Eq, Show)

rickard :: UserProfile
rickard =
  UserProfile
    { username = "rickard",
      age = 34,
      active = True,
      interests = ["Programming", "Problem Solving", "Teaching"]
    }

{-
profileToString :: UserProfile -> String
profileToString UserProfile {username, age, active, interests} =
  let ageString = show age
      activeString = if active then "active" else "not active"
      interestsString = intercalate ", " interests
   in mconcat
        [ username,
          " (",
          ageString,
          "y, ",
          activeString,
          ") is interested in: ",
          interestsString
        ]
-}

data StringAndLength = StringAndLength
  { lengthOfString :: Int,
    string :: String
  }
  deriving (Eq, Show)

stringAndLength :: String -> StringAndLength
stringAndLength str = StringAndLength {lengthOfString = length str, string = str}

data Product = Product
  { name :: String,
    price :: Double,
    taxationRate :: Double
  }
  deriving (Eq, Show)

totalPrice :: Product -> Double
totalPrice Product {name, price, taxationRate} =
  price + (price * taxationRate)

myProduct :: Product
myProduct = Product {name = "Bio Cucumber", price = 5, taxationRate = 0.2}