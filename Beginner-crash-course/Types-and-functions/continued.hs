idk :: (Num a, Ord a) => a -> a
-- since the input can be int, double...(polimorphic)
-- we constrain the value a which will be out input to the functionallities of the Num and Ord classes
idk x =
    if (x < 10) --Ord class
        then negate x -- Num class
        else x + 10 -- Num class

idk' :: (Num a, Ord a) => a -> a
-- <constrain the variable a> => <input> -> <output>
idk' x =
    case (x < 10) of
        True -> negate x
        False -> x + 10
        
preferJ :: String -> String -> String
preferJ x y =
    if (elem 'j' x)
        then x
        else y