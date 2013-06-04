-- main function to encrypt strings
encryptString :: Integer -> Integer -> [Char] -> [Integer]
encryptString e n ms
  | getNextPossibleCharBlockSize n == 0 = [-1]
  | otherwise = encryptBlocks e n (getMessageBlocks ms (getNextPossibleCharBlockSize n))

-- bs = block list
encryptBlocks :: Integer -> Integer -> [Integer] -> [Integer]
encryptBlocks e n bs
  | (length bs) == 1 = [encryptExec e n (head bs)]
  | otherwise = [encryptExec e n (head bs)] ++ (encryptBlocks e n (tail bs))

-- build list of message blocks, b = block size
getMessageBlocks :: String -> Int -> [Integer]
getMessageBlocks m b
  | (length m) <= b = [fromIntegral (charBlockToIntBlock m 0)]
  | otherwise = [fromIntegral (charBlockToIntBlock (take b m) 0)] ++ (getMessageBlocks (drop b m) b)

-- cb = char block, e = exponent (start with 0)
charBlockToIntBlock :: [Char] -> Int -> Int
charBlockToIntBlock cb e
  | (length cb) == 1 = (ord (head cb)) * (256^e)
  | otherwise = ((ord (head cb)) * (256^e)) + charBlockToIntBlock (tail cb) (e+1)

  -- get list of char blocks to encrypt / decrypt
-- info: chars are stored as utf8 (8bits)
getCharBlocks :: String -> Int -> [[Char]]
getCharBlocks m n
  | (length m) <= (getNextPossibleCharBlockSize n) = [m]
  | otherwise = [(take (getNextPossibleCharBlockSize n) m)] ++ (getCharBlocks (drop (getNextPossibleCharBlockSize n) m) n)

-- if n < m -> RSA not possible, returns number of chars, not actual size
getNextPossibleCharBlockSize :: (Integral b, Num a, Ord a) => a -> b
getNextPossibleCharBlockSize n = snd (getNextSmallerPowerOfN 256 n)

-- returns last power of b which is still smaller than x
getNextSmallerPowerOfN :: (Integral b, Num t, Ord t) => t -> t -> (t, b)
getNextSmallerPowerOfN b x = getNextSmallerPowerOfN2 b x 1

getNextSmallerPowerOfN2 :: (Integral b, Num t, Ord t) => t -> t -> b -> (t, b)
getNextSmallerPowerOfN2 b x e
  | x > (b^e) = getNextSmallerPowerOfN2 b x (e+1)
  | x == (b^e) = (b^e,e)
  | otherwise = (b^(e-1),(e-1))

-- executes encryption
encryptExec :: Integer -> Integer -> Integer -> Integer
encryptExec e n m = powerMod m e n