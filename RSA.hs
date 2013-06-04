-- # Define RSA module and its public functions
module RSA
(  generateKeyPair
,  encrypt
,  decrypt
) where

-- # import required external modules
import System.Random
import Control.Monad.Fix
import Data.Int
import Data.Bits
import Data.Char
import System.IO


-- # private functions

-- checks if x is prime
-- all even numbers equal directly to False
isPrime :: Integer -> Bool
isPrime 1 = False
isPrime 2 = True
isPrime x | even x == False && (not ((mod x 5) == 0)) && (not ((mod x 3) == 0)) && (not ((mod x 7) == 0)) = isNotDivisor (getDivisorList x) x
	  | otherwise = False

-- returns a list of possible not even divisors for x which are smaller then sqrt of x
getDivisorList :: Integer -> [Integer]
getDivisorList x = [y | y <- 2:filter (not.even) [2.. (round (sqrt (fromInteger x)))]]

-- checks wether there is an divisor or not
-- returns False as soon as one divisor is found
-- returns True if no divisor is found
isNotDivisor :: [Integer] -> Integer -> Bool
isNotDivisor [] _ = True
isNotDivisor (w:ws) x | (mod x w) == 0 = False
		      | otherwise = isNotDivisor ws x
		
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

-- executes encryption
encryptExec :: Integer -> Integer -> Integer -> Integer
encryptExec e n m = powerMod m e n

-- executes decryption
decryptExec :: Integer -> Integer -> Integer -> Integer
decryptExec d n c = powerMod c d n

-- Interaction to get fitting primes
-- Control.Monad.Fix idea from StackOverflow: http://stackoverflow.com/a/13301611
enterPrimes :: Integer -> IO (Integer, Integer)
enterPrimes e =
    fix $ \again -> do
        putStrLn "Enter first prime: "
        prime <- getLine
        let p = read prime
        putStrLn "Enter second prime: "
        prime <- getLine
        let q = read prime :: Integer
            phi = (p-1)*(q-1)
        --if ((gcd e phi) == 1 && (isPrime p) && (isPrime q)) then
        if ((gcd e phi) == 1) then
           return (p, q)
        else
           again

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

-- tests if private key d meets all the criteria
testD :: Int -> Int -> Int -> Int -> Bool
testD e d p q = mod (e*d) ((p-1)*(q-1)) == 1


-- Interaction to generate key pair which are stored in pub.key/priv.key
generateKeyPair :: IO ()
generateKeyPair =
    do putStrLn "--------------------------------------------------------------------------"
       putStrLn "Key generation started (e = 65537): "
       writeFile ("pub.key") ""
       writeFile ("priv.key") ""
       let e = 65537 :: Integer
       putStrLn "NOTICE: If the primes don't match the requirements [(gcd e phi) <> 1]"
       putStrLn "you will have to enter different ones."
       putStrLn "Enter exponent (leave blank for default [65537])"
       exp <- getLine
       let e
            | exp == "" = 65537 :: Integer
            | otherwise = read exp :: Integer
       primes <- enterPrimes e :: IO (Integer, Integer)
       let p = fst primes
           q = snd primes :: Integer
           n = p*q
           phi = (p-1)*(q-1)
           d = inverseMod e phi :: Integer
           resultPub = (e, n)
           resultPriv = (d, n)
       writeFile ("pub.key") (show resultPub)
       writeFile ("priv.key") (show resultPriv)
       putStrLn ("Key pair saved in pub.key and priv.key")

-- interaction for encryption process
encrypt :: IO ()
encrypt =
    do putStrLn "Please enter fileName which contains public key (e.g. pub.key): "
       pubKeyFileName <- getLine
       stringFileContents <- readFile (pubKeyFileName)
       let en = read stringFileContents :: (Integer, Integer)
           e = fst en
           n = snd en
       putStrLn "Please enter message to encrypt: "
       message <- getLine
       putStr "Encrypted text (as int blocks): "
       putStrLn (show (encryptString e n message))

-- interaction for decryption process
decrypt :: IO ()
decrypt =
    do putStrLn "Please enter fileName which contains private key (e.g. priv.key): "
       privKeyFileName <- getLine
       stringFileContents <- readFile (privKeyFileName)
       let dn = read stringFileContents :: (Integer, Integer)
           d = fst dn
           n = snd dn
       putStrLn "Please enter message to decrypt (as int blocks): "
       cipher <- getLine
       let c = read cipher :: [Integer]
       putStr "Decrypted text: "
       putStrLn (show (decryptString d n c))


--------------------------------------------------
--	Testfälle
--------------------------------------------------

testPowerMod :: Integer -> Integer -> Integer -> Bool
testPowerMod b e m = powerMod b e m == mod (b^e) m

--testEncryptExec
--testDecryptExec
--testEncryptString
--testDecryptString