-- modular multiplicative inverse
inverseMod :: Integer -> Integer -> Integer
inverseMod e phi =
  (x + phi) `mod` phi
  where
    (z, (x, y)) = ((gcd e phi),euclid e phi)

-- extended euclidean algorithm
euclid :: Integer -> Integer -> (Integer, Integer)
euclid 0 n = (0,1)
euclid e n
  | n == 0 = (1,0)
  | otherwise = (t, s-q*t)
    where
      (q, r) = quotRem e n
      (s, t) = euclid n r