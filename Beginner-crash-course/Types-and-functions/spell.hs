spell :: Int -> String
-- <name> has a type <input type(concrete)> -> <output type(concrete)>
spell x =
    case x of
        1 -> "one"
        2 -> "two"
        3 -> "three"
        4 -> "four"
        _ -> "other number"

spell' :: String -> Int
spell' name =
    case name of
        "Martin" -> 20
        "Petar" -> 22
        _ -> -1