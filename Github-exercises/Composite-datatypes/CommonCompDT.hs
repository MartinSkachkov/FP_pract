-- Maybe a is a type that represents the existence or non-existence of a value of type a
-- data Maybe a
--  = Nothing
--  | Just a

data ResourceLoadStatus
  = NotYetLoaded
  | Loaded Resource
  deriving (Eq, Show)

newtype Resource = Resource String
  deriving (Eq, Show)

resourceLoadStatusToMaybe :: ResourceLoadStatus -> Maybe Resource
resourceLoadStatusToMaybe NotYetLoaded = Nothing
resourceLoadStatusToMaybe (Loaded resource) = Just resource

newtype Username = Username String
  deriving (Eq, Show)

newtype Email = Email String
  deriving (Eq, Show)

newtype FullName = FullName String
  deriving (Eq, Show)

newtype PhoneNumber = PhoneNumber String
  deriving (Eq, Show)

data User = User
  { username :: Username,
    email :: Email,
    fullName :: Maybe FullName,
    phone :: Maybe PhoneNumber
  }
  deriving (Eq, Show)

user :: User
user = User {username = Username "gonz", email = Email "rickard.andersson@quanterall.com", fullName = Just (FullName "Rickard Andersson"), phone = Just (PhoneNumber "555 363 22 34")}

userWithoutPhone :: User
userWithoutPhone = user {phone = Nothing}

showPhoneNumber :: User -> Maybe PhoneNumber
showPhoneNumber User {phone = Nothing} = Nothing
showPhoneNumber User {phone} = phone

pureMaybe :: a -> Maybe a
pureMaybe val = Just val

foldMaybe :: b -> (a -> b) -> Maybe a -> b
foldMaybe val func Nothing = val
foldMaybe val func (Just a) = func a

mapMaybe :: (a -> b) -> Maybe a -> Maybe b
mapMaybe func (Just a) = Just (func a)
mapMaybe func Nothing = Nothing

applyMaybe :: Maybe (a -> b) -> Maybe a -> Maybe b
applyMaybe (Just func) (Just a) = Just (func a)
applyMaybe Nothing (Just a) = Nothing
applyMaybe (Just func) Nothing = Nothing

bindMaybe :: (a -> Maybe b) -> Maybe a -> Maybe b
bindMaybe func (Just a) = func a
bindMaybe func Nothing = Nothing

-- Tuples
-- A tuple is an ad-hoc collection of values that can be of different types

tuple :: (Int, String)
tuple = (42, "Forty-Two")

tuple' :: (Int, String, Bool)
tuple' = (42, "Forty-Two", False)

tuple'' :: (Int, String, Bool, Float)
tuple'' = (42, "Forty-Two", False, 1337.0)

-- In many ways a tuple is the same as a record, except we are not naming the different components or the constructor.

-- Lists
-- data [] a
--  = []
--  | a : [a]

data List a
  = EmptyList -- This is commonly called `Nil`
  | -- | `a` here is commonly called the "head" of the list, and the list it is connected to is
    -- commonly called the "tail".
    Prepend a (List a) -- This is commonly called `Cons`

maybeFirstElement :: [a] -> Maybe a
maybeFirstElement (a : _) = Just a
maybeFirstElement [] = Nothing

maybeFirstElement' :: [a] -> Maybe a
maybeFirstElement' list = case list of
  a : _ -> Just a
  [] -> Nothing

maybeFirstTwoElements :: [a] -> Maybe (a, a)
maybeFirstTwoElements (a : b : _) = Just (a, b)
maybeFirstTwoElements [] = Nothing

maybeFirstTwoElements' :: [a] -> Maybe (a, a)
maybeFirstTwoElements' list = case list of
  a : b : _ -> Just (a, b)
  [] -> Nothing

maybeFirstAndRest :: [a] -> Maybe (a, [a])
maybeFirstAndRest (a : rest) = Just (a, rest)
maybeFirstAndRest _anyOtherCase = Nothing

-- We can also match to an exact structure of a list
maybeExactlyTwoElements :: [a] -> Maybe (a, a)
maybeExactlyTwoElements [a, b] = Just (a, b)
maybeExactlyTwoElements _anyOtherCase = Nothing