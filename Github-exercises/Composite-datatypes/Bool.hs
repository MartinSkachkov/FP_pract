{-# LANGUAGE EmptyDataDeriving #-}
-- cover all cases!
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}
-- warn about incomplete patterns v2
{-# OPTIONS_GHC -fwarn-incomplete-uni-patterns #-}
-- write all your toplevel signatures!
{-# OPTIONS_GHC -fwarn-missing-signatures #-}
-- use different names!
{-# OPTIONS_GHC -fwarn-name-shadowing #-}
-- use all your pattern matches!
{-# OPTIONS_GHC -fwarn-unused-matches #-}

data MyBool = MyTrue | MyFalse
  deriving (Show)

-- top-level pattern matching
and' :: MyBool -> MyBool -> MyBool
and' MyTrue MyTrue = MyTrue
and' _ _ = MyFalse

-- just pattern matching
and'' :: MyBool -> MyBool -> MyBool
and'' bool1 bool2 =
  case (bool1, bool2) of
    (MyTrue, MyTrue) -> MyTrue
    (_, _) -> MyFalse

or' :: MyBool -> MyBool -> MyBool
or' MyTrue _ = MyTrue
or' _ MyTrue = MyTrue
or' _ _ = MyFalse

or'' :: MyBool -> MyBool -> MyBool
or'' bool1 bool2 =
  case (bool1, bool2) of
    (MyTrue, _) -> MyTrue
    (_, MyTrue) -> MyTrue
    (_, _) -> MyFalse

exclusiveOr :: MyBool -> MyBool -> MyBool
exclusiveOr MyFalse MyFalse = MyFalse
exclusiveOr MyTrue MyFalse = MyTrue
exclusiveOr MyFalse MyTrue = MyTrue
exclusiveOr MyTrue MyTrue = MyFalse

exclusiveOr' :: MyBool -> MyBool -> MyBool
exclusiveOr' bool1 bool2 =
  case (bool1, bool2) of
    (MyFalse, MyFalse) -> MyFalse
    (MyTrue, MyFalse) -> MyTrue
    (MyFalse, MyTrue) -> MyTrue
    (MyTrue, MyTrue) -> MyFalse

bool :: Int -> Int -> MyBool -> Int
bool x y boolVal =
  case boolVal of
    MyTrue -> x
    MyFalse -> y

bool' :: Int -> Int -> Bool -> Int
bool' x y boolVal =
  if boolVal
    then x
    else y

bool'' :: Int -> Int -> Bool -> Int
bool'' x _ True = x
bool'' _ y False = y