-- main function to decrypt strings
decryptString :: Integer -> Integer -> [Integer] -> [Char]
decryptString d n cs
  | (getNextPossibleCharBlockSize n) == 0 = [' ']
  | otherwise = decryptBlocks d n cs

-- bs = blocklist
decryptBlocks :: Integer -> Integer -> [Integer] -> [Char]
decryptBlocks d n bs
  | (length bs) == 1 = intBlockToCharBlock (fromIntegral (decryptExec d n (head bs)))
  | otherwise = intBlockToCharBlock (fromIntegral (decryptExec d n (head bs))) ++ (decryptBlocks d n (tail bs))

-- ib = int block, m = modulo, b = block size (chars)
intBlockToCharBlock :: Int-> [Char]
intBlockToCharBlock ib
  | ib == 0 = []
  | otherwise = [(chr (mod ib 256))] ++ intBlockToCharBlock (shiftR ib 8)
