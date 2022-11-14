{-# LANGUAGE LambdaCase #-}

module Matrix where

data Three = Zero | One | Two
  deriving (Eq, Show)

flipThree :: Three -> Three
flipThree Zero = Two
flipThree One = One
flipThree Two = Zero

type Thrice a = Three -> a

thriceToTriple :: Thrice a -> (a, a, a)
thriceToTriple t = (t Zero, t One, t Two)

thrice :: a -> a -> a -> Thrice a
thrice x y z t =
  case t of
    Zero -> x
    One -> y
    Two -> z

newtype Matrix a = MkMatrix {getMatrix :: Thrice (Thrice a)}

matrix ::
  a ->
  a ->
  a ->
  a ->
  a ->
  a ->
  a ->
  a ->
  a ->
  Matrix a
matrix x0 x1 x2 y0 y1 y2 z0 z1 z2 =
  MkMatrix $ \case
    Zero -> thrice x0 x1 x2
    One -> thrice y0 y1 y2
    Two -> thrice z0 z1 z2

-- MkMatrix $ \case
--   ...cases...
-- is exactly the same as
-- MkMatrix $ \i ->
--   case i of
--     ...cases...
-- It requires the `LambdaCase` language extension - it's enabled at the top of the file

constantMatrix :: a -> Matrix a
constantMatrix val = matrix val val val val val val val val val

diagonalMatrix :: a -> Matrix (Maybe a)
diagonalMatrix val = MkMatrix $ \i j -> if i == j then Just val else Nothing

otherDiagonalMatrix :: a -> Matrix (Maybe a)
otherDiagonalMatrix val = MkMatrix $ \i j -> if i == Zero && j == Two || i == One && j == One || i == Two && j == Zero
  then Just val
  else Nothing 
  
addMatrix :: Matrix Integer -> Matrix Integer -> Matrix Integer
addMatrix m1 m2 = MkMatrix $ \i j -> getMatrix m1 i j + getMatrix m2 i j

m1 :: Matrix Integer
m1 =
  matrix
    1
    2
    3
    4
    5
    6
    7
    8
    9

m2 :: Matrix Integer
m2 =
  matrix
    (-1)
    (-2)
    (-3)
    (-4)
    (-5)
    (-6)
    (-7)
    (-8)
    (-9)

showMatrix :: (a -> String) -> Matrix a -> String
showMatrix f (MkMatrix m) =
  f (m Zero Zero)
    ++ " "
    ++ f (m Zero One)
    ++ " "
    ++ f (m Zero Two)
    ++ " , "
    ++ f
      (m One Zero)
    ++ " "
    ++ f (m One One)
    ++ " "
    ++ f (m One Two)
    ++ " , "
    ++ f
      (m Two Zero)
    ++ " "
    ++ f (m Two One)
    ++ " "
    ++ f (m Two Two)

ix :: Three -> Three -> Matrix a -> a
ix i j m = getMatrix m i j

getRow :: Three -> Matrix a -> Thrice a
getRow row m = getMatrix m row

getCol :: Three -> Matrix a -> Thrice a
getCol col m = thrice (getMatrix m Zero col) (getMatrix m One col) (getMatrix m Two col) 

getDiag :: Matrix a -> Thrice a -- i == j
getDiag m = thrice (getMatrix m Zero Zero) (getMatrix m One One) (getMatrix m Two Two)

getOtherDiag :: Matrix a -> Thrice a
getOtherDiag m =  thrice (getMatrix m Zero Two) (getMatrix m One One) (getMatrix m Two Zero)

transpose :: Matrix a -> Matrix a
transpose = error "not implemented"

foldThriceWith :: (a -> a -> a) -> Thrice a -> a
foldThriceWith = error "not implemented"

foldMatrixWith :: (a -> a -> a) -> Matrix a -> a
foldMatrixWith = error "not implemented"

eqMatrix :: (a -> a -> Bool) -> Matrix a -> Matrix a -> Bool
eqMatrix = error "not implemented"

imapThrice :: (Three -> a -> b) -> Thrice a -> Thrice b
imapThrice = error "not implemented"

mapThrice :: (a -> b) -> Thrice a -> Thrice b
mapThrice = error "not implemented"

imapMatrix :: (Three -> Three -> a -> b) -> Matrix a -> Matrix b
imapMatrix = error "not implemented"

mapMatrix :: (a -> b) -> Matrix a -> Matrix b
mapMatrix = error "not implemented"

place :: Three -> Three -> a -> Matrix a -> Matrix a
place = error "not implemented"

concatThriceWith :: String -> Thrice String -> String
concatThriceWith = error "not implemented"

concatMatrixWith :: String -> String -> Matrix String -> String
concatMatrixWith = error "not implemented"

showMatrixComposition :: (a -> String) -> Matrix a -> String
showMatrixComposition = error "not implemented"

instance Show a => Show (Matrix a) where
  show = showMatrix show

{-
type GenericMatrix =

foldGenericMatrix ::
mapGenericMatrix ::
multGenericMatrix ::
-}
