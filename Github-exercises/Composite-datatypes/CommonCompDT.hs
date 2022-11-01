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