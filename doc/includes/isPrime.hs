-- checks if x is prime
-- all even numbers equal directly to False
isPrime :: Integer -> Bool
isPrime 1 = False
isPrime 2 = True
isPrime x | even x == False && (not ((mod x 5) == 0)) && (not ((mod x 3) == 0)) && (not ((mod x 7) == 0)) = isNotDivisor (getDivisorList x) x
          | otherwise = False

-- returns a list of possible divisors for x
getDivisorList :: Integer -> [Integer]                     
getDivisorList x = [y | y <- 2:filter (not.even) [2.. (round (sqrt (fromInteger x)))]]

-- checks wether there is an divisor or not
-- returns False as soon as one divisor is found
-- returns True if no divisor is found
isNotDivisor :: [Integer] -> Integer -> Bool
isNotDivisor [] _ = True
isNotDivisor (w:ws) x | (mod x w) == 0 = False
                      | otherwise = isNotDivisor ws x