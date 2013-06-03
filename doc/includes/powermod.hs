-- modular exponentiation
powerMod :: Integer -> Integer -> Integer -> Integer
powerMod b e m = powerModExec b (toBin e) m 1

 -- modular exponentiation execution
powerModExec :: Integer -> [Integer] -> Integer -> Integer -> Integer
powerModExec b e m c
    | e == [] = c
    | head e == 1 = powerModExec b (tail e) m ((c^2 `mod ` m)*b `mod` m)
    | otherwise = powerModExec b (tail e) m (c^2 `mod` m)

-- convert Integer to an Integer list which represents original Integer in binary
toBin :: Integer -> [Integer]
toBin 0 = [0]
toBin 1 = [1]
toBin n
    | n `mod` 2 == 0 = toBin (n `div` 2) ++ [0]
    | otherwise = toBin (n `div` 2) ++ [1]