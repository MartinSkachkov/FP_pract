-- newtypes are like data with **only one** constructor and **only one** field
-- new type and the type of the field are in direct correspondance

--       type    constructor
newtype Source = Source String
  -- \^ This constructor, `Source`, takes one argument, a `String`.
  -- When that argument is supplied to it, we get a value of type `Source`.
  deriving (Eq, Show)

--       type         constructor
newtype Destination = Destination String
  deriving (Eq, Show)

--       type         constructor
newtype CopyPattern = CopyPattern String
  deriving (Eq, Show)

filteredCopy :: Source -> Destination -> CopyPattern -> IO ()
filteredCopy (Source source) (Destination destination) (CopyPattern copyPattern) = undefined
-- ^ Note how we can deconstruct these wrappers just like with other forms of data definitions. This
-- is a very useful thing to do when we effectively want to be working with the strings that these
-- types contain. It means that while we cannot blindly pass strings **to** this function, we still
-- have the ease of working with the wrapped types inside of it.

newtype Meters = Meters Float
  deriving (Eq, Show)

addMeters :: Meters -> Meters -> Meters
addMeters (Meters x) (Meters y) = Meters (x + y)

newtype Kilometers = Kilometers Float
  deriving (Eq, Show)

metersToKilometers :: Meters -> Kilometers
metersToKilometers (Meters m) = Kilometers (m / 1000)

newtype Username = Username String
  deriving (Eq, Show)

usernameLength :: Username -> Int
usernameLength (Username name) = length name