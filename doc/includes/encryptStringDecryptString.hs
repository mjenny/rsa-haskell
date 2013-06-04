-- if n < m -> RSA not possible, returns number of chars, not actual size
getNextPossibleCharBlockSize :: (Integral b, Num a, Ord a) => a -> b
getNextPossibleCharBlockSize n = snd (getNextSmallerPowerOfN 256 n)

-- returns last power of n which is still smaller than x
getNextSmallerPowerOfN :: (Integral b, Num t, Ord t) => t -> t -> (t, b)
getNextSmallerPowerOfN n x = getNextSmallerPowerOfNExec n x 1

getNextSmallerPowerOfNExec :: (Integral b, Num t, Ord t) => t -> t -> b -> (t, b)
getNextSmallerPowerOfNExec n x e
  | x > (n^e) = getNextSmallerPowerOfNExec n x (e+1)
  | x == (n^e) = (n^e,e)
  | otherwise = (n^(e-1),(e-1))
