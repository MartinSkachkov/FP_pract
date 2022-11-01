-- Note how we now have a type variable on the left side of = which means that when we refer to the type it will take a type name.
-- If we were holding a Int, for example, the type is Holder Int.
-- The corresponding constructor call (or pattern match) is Holder value, where value is of type Int.

data HttpResponse a = HttpResponse
  { status :: HttpStatus,
    headers :: [HttpHeader],
    body :: a
  }
  deriving (Eq, Show)

data HttpStatus
  = Ok Int
  | ClientError Int
  | ServerError Int
  deriving (Eq, Show)

data HttpHeader = HttpHeader
  { headerName :: String,
    headerValue :: String
  }
  deriving (Eq, Show)

data SomeAmountOf a
  = None
  | One a
  | CoupleOf a a
  | BunchOf a a [a]
  deriving (Eq, Show)

none :: SomeAmountOf Int -- 'a' is replaced by Int
none = None

one :: SomeAmountOf Int
one = One 42

coupleOf :: SomeAmountOf Int
coupleOf = CoupleOf 42 1337

bunchOf :: SomeAmountOf Int
bunchOf = BunchOf 42 1337 [1, 2, 3]

data Holder a
  = NoValue
  | Holder a
  deriving (Eq, Show)

val1 :: Holder Int
val1 = Holder 45

val2 :: Holder String
val2 = Holder "Marto"

pureHolder :: a -> Holder a
pureHolder val = Holder val

foldHolder :: (a -> b) -> Holder a -> b
foldHolder func (Holder a) = func a

-- foldHolder (+ 1) (Holder 42) -> calls func 42, but since func is defined to increment by one we get 43
-- foldHolder length (Holder "hello") -> call func "hello", but since func is actually length we get (length "hello") which gives 5

foldHolder' :: (a -> b) -> b -> Holder a -> b
foldHolder' func b (Holder a) = func a
foldHolder' func b NoValue = b

mapHolder :: (a -> b) -> Holder a -> Holder b
mapHolder func (Holder a) = Holder (func a)

applyHolder :: Holder (a -> b) -> Holder a -> Holder b
applyHolder (Holder func) (Holder a) = Holder (func a)

bindHolder :: (a -> Holder b) -> Holder a -> Holder b
bindHolder func (Holder a) = func a

-- bindHolder (\v -> Holder $ length v) (Holder "hello")
-- bindHolder (\v -> Holder $ v + 1) (Holder 42)