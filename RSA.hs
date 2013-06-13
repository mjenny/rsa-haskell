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

-- # type definitions
type PubKey = (Integer, Integer)
type PrivKey = (Integer, Integer)
type Prime = Integer

-- # private functions

-- define a three tuple
fst3 :: (a, b, c) -> a
fst3 (a,b,c) = a

snd3 :: (a,b,c) -> b
snd3 (a,b,c) = b

thd3 :: (a, b, c) -> c
thd3 (a,b,c) = c

-- checks if x is prime
-- all even numbers equal directly to False
isPrime :: Prime -> Bool
isPrime 1 = False
isPrime 2 = True
isPrime x | even x == False && (not ((mod x 5) == 0)) && (not ((mod x 3) == 0)) && (not ((mod x 7) == 0)) = isNotDivisor (getDivisorList x) x
	  | otherwise = False

-- returns a list of possible not even divisors for x which are smaller then sqrt of x
getDivisorList :: Prime -> [Integer]
getDivisorList x = [y | y <- 2:filter (not.even) [2.. (round (sqrt (fromInteger x)))]]

-- checks wether there is an divisor or not
-- returns False as soon as one divisor is found
-- returns True if no divisor is found
isNotDivisor :: [Integer] -> Prime -> Bool
isNotDivisor [] _ = True
isNotDivisor (w:ws) x | (mod x w) == 0 = False
		      | otherwise = isNotDivisor ws x
		
-- modular multiplicative inverse
inverseMod :: Integer -> Integer -> Integer
inverseMod e phi =
  (x + phi) `mod` phi
  where
    (x, y) = euclid e phi

-- extended euclidean algorithm
euclid :: Integer -> Integer -> (Integer, Integer)
euclid 0 _ = (0,1)
euclid _ 0 = (1,0)
euclid e n = (t, s-q*t)
    where
      (q, r) = quotRem e n
      (s, t) = euclid n r

-- modular exponentiation
powerMod :: Integer -> Integer -> Integer -> Integer
powerMod b e m = powerModExec b (toBin e) m 1

 -- modular exponentiation execution
powerModExec :: Integer -> [Integer] -> Integer -> Integer -> Integer
powerModExec _ [] _ c = c
powerModExec b e m c
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

-- Interaction to get fitting primes and exponent
-- Control.Monad.Fix idea from StackOverflow: http://stackoverflow.com/a/13301611
enterExpPrimes :: IO (Integer, Prime, Prime)
enterExpPrimes =
    fix $ \again -> do
        putStrLn "Enter exponent (leave blank for default [65537])"
        exp <- getLine
        let e
              | exp == "" = 65537 :: Integer
              | otherwise = read exp :: Integer
        putStrLn "Enter first prime: "
        prime <- getLine
        let p = read prime :: Prime
        putStrLn "Enter second prime: "
        prime <- getLine
        let q = read prime :: Prime
            phi = (p-1)*(q-1)
        if ((gcd e phi) == 1 && (isPrime p) && (isPrime q)) then
        -- ignore isPrime to use large possible primes generated with sage (http://www.sagemath.org)
        --if ((gcd e phi) == 1) then
           return (e, p, q)
        else
           do putStrLn "ERROR: Exponent and primes don't match the requirements [(gcd e phi]) <> 1]"
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
intBlockToCharBlock 0 = []
intBlockToCharBlock ib = [(chr (mod ib 256))] ++ intBlockToCharBlock (shiftR ib 8)

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

-- returns last power of n which is still smaller than x
getNextSmallerPowerOfN :: (Integral b, Num t, Ord t) => t -> t -> (t, b)
getNextSmallerPowerOfN n x = getNextSmallerPowerOfNExec n x 1

-- execute getNextSmallerPowerOfN
getNextSmallerPowerOfNExec :: (Integral b, Num t, Ord t) => t -> t -> b -> (t, b)
getNextSmallerPowerOfNExec n x e
  | x > (n^e) = getNextSmallerPowerOfNExec n x (e+1)
  | x == (n^e) = (n^e,e)
  | otherwise = (n^(e-1),(e-1))

-- # public functions
-- Interaction to generate key pair which are stored in pub.key/priv.key
generateKeyPair :: IO ()
generateKeyPair =
    do putStrLn "---------------------------------------------------------------------------"
       putStrLn "-------------------------- Key generation started -------------------------"
       putStrLn "---------------------------------------------------------------------------"
       writeFile ("pub.key") ""
       writeFile ("priv.key") ""
       putStrLn "NOTICE: If the primes don't match the requirements [(gcd e phi) <> 1]"
       putStrLn "In this case you will have to enter different ones."
       expPrimes <- enterExpPrimes :: IO (Integer, Prime, Prime)
       let e = fst3 expPrimes
           p = snd3 expPrimes
           q = thd3 expPrimes
           n = p*q
           phi = (p-1)*(q-1)
           d = inverseMod e phi :: Integer
           resultPub = (e, n) :: PubKey
           resultPriv = (d, n) :: PrivKey
       writeFile ("pub.key") (show resultPub)
       writeFile ("priv.key") (show resultPriv)
       putStrLn ("Key pair saved in pub.key and priv.key")

-- interaction for encryption process
encryptOld :: IO ()
encryptOld =
    do putStrLn "Please enter fileName which contains public key (e.g. pub.key): "
       pubKeyFileName <- getLine
       stringFileContents <- readFile (pubKeyFileName)
       let pub = read stringFileContents :: PubKey
           e = fst pub
           n = snd pub
       putStrLn "Please enter message to encrypt: "
       message <- getLine
       putStr "Encrypted text (as int blocks): "
       putStrLn (show (encryptString e n message))

-- interaction for encryption process
encrypt :: IO ()
encrypt =
    do putStrLn "Please enter fileName which contains public key (e.g. pub.key): "
       pubKeyFileName <- getLine
       stringFileContents <- readFile (pubKeyFileName)
       let pub = read stringFileContents :: PubKey
           e = fst pub
           n = snd pub
       putStrLn "Please enter fileName which contains text to encrypt (plainText.txt): "
       fileName <- getLine
       message <- readFile (fileName)
       putStrLn "Please enter fileName to store encrypted text (encryptedText.txt): "
       fileName <- getLine
       writeFile (fileName) ""
       writeFile (fileName) (show (encryptString e n message))
       putStr "Encrypted text stored in "
       putStrLn (fileName)

-- interaction for decryption process
decryptOld :: IO ()
decryptOld =
    do putStrLn "Please enter fileName which contains private key (e.g. priv.key): "
       privKeyFileName <- getLine
       stringFileContents <- readFile (privKeyFileName)
       let priv = read stringFileContents :: PrivKey
           d = fst priv
           n = snd priv
       putStrLn "Please enter message to decrypt (as int blocks): "
       cipher <- getLine
       let c = read cipher :: [Integer]
       putStr "Decrypted text: "
       putStrLn (decryptString d n c)

-- interaction for decryption process
decrypt :: IO ()
decrypt =
    do putStrLn "Please enter fileName which contains private key (e.g. priv.key): "
       privKeyFileName <- getLine
       stringFileContents <- readFile (privKeyFileName)
       let priv = read stringFileContents :: PrivKey
           d = fst priv
           n = snd priv
       putStrLn "Please enter fileName which contains text to decrypt (encryptedText.txt): "
       fileName <- getLine
       cipher <- readFile (fileName)
       let c = read cipher :: [Integer]
       putStrLn "Please enter fileName to store decrypted text (decryptedText.txt): "
       fileName <- getLine
       writeFile (fileName) ""
       writeFile (fileName) (decryptString d n c)
       putStr "Decrypted text stored in "
       putStrLn (fileName)

----------------------------------------------
--	test cases
--------------------------------------------------

testPowerMod :: Integer -> Integer -> Integer -> Bool
testPowerMod b e m = (powerMod b e m) == (mod (b^e) m)

testEncryptExec :: Integer -> Integer -> Integer -> Integer -> Bool
testEncryptExec e n m c = (encryptExec e n m) == c

testDecryptExec :: Integer -> Integer -> Integer -> Integer -> Bool
testDecryptExec d n c m = (decryptExec d n c) == m

testEncryptDecryptString :: String -> Integer -> Integer -> Integer -> Bool
testEncryptDecryptString m e d n = m == (decryptString d n (encryptString d n m))
