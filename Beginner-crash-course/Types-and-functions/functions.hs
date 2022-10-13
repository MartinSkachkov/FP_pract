in_range :: Int -> Int -> Int -> Bool
--in_range min max x =
--    x >= min && x <= max

--in_range 0.5 1.5 1 //error(floats)
--in_range 0 5 3 //OK(integers)

in_range min max x =
    let in_lower_bound = min <= x
        in_upper_bound = x <= max
    in
        (in_lower_bound && in_upper_bound)