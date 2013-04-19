-- modular multiplicative inverse
inverseMod :: Integer -> Integer -> Integer
inverseMod e n =
  (x + n) `mod` n
  where
    (z, (x, y)) = ((gcd e n),euclid e n)

-- extended euclidean algorithm    
euclid :: Integer -> Integer -> (Integer, Integer)
euclid 0 n = (0,1)
euclid e n
  | n == 0 = (1,0)
  | otherwise = (t, s-q*t)
    where
      (q, r) = quotRem e n
      (s, t) = euclid n r
