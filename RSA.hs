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


-- # private functions

-- # get Tupel of two large primes
--getPrimes
{-|
	* this is a multi-line comment *
	
	Possibility to generate large prime numbers:
	1. create candidate: odd number n of appropriate size
	2. test n for primality (z.B. Miller-Rabin?)
	3. if n is no prime, return to step 1

-}

-- Source: http://stackoverflow.com/questions/13300533/haskell-prime-number-generator-according-to-bits-for-very-large-numbers

{-
rndPrime :: Int -> IO Integer
rndPrime bits =
    fix $ \again -> do
        x <- fmap (.|. 1) $ randomRIO (2^(bits - 1), 2^bits - 1)
        if millerRabin x then return x else again

rndPrimes :: Int -> IO (Integer, Integer)
rndPrimes bits = do
    p <- rndPrime bits
    fix $ \again -> do
        q <- rndPrime bits
        if p /= q then return (p, q) else again

millerRabin ::
millerRabin x = (mod (2^(x-1)) x) not (1)
-}

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
		
-- extended euclidean algorithm
euclid :: Integer -> Integer -> (Integer, Integer)
euclid 0 n = (0,1)
euclid e n
  | n == 0 = (1,0)
  | otherwise = (t, s-q*t)
    where
      (q, r) = quotRem e n
      (s, t) = euclid n r
		
-- modular multiplicative inverse
inverseMod :: Integer -> Integer -> Integer
inverseMod e n =
  (x + n) `mod` n
  where
    (z, (x, y)) = ((gcd e n),euclid e n)

-- convert Integer to an Integer list which represents original Integer in binary
toBin :: Integer -> [Integer]
toBin 0 = [0]
toBin 1 = [1]
toBin n
    | n `mod` 2 == 0 = toBin (n `div` 2) ++ [0]
    | otherwise = toBin (n `div` 2) ++ [1]

-- modular exponentiation
powerMod :: Integer -> Integer -> Integer -> Integer
powerMod b e m = powerModExec b (toBin e) m 1

 -- modular exponentiation execution
powerModExec :: Integer -> [Integer] -> Integer -> Integer -> Integer
powerModExec b e m c
    | e == [] = c
    | head e == 1 = powerModExec b (tail e) m ((c^2 `mod ` m)*b `mod` m)
    | otherwise = powerModExec b (tail e) m (c^2 `mod` m)

-- use to save performance while doing millerRabin
-- shiftR = bitwise right shift
--squareMultiply b e =

encryptExec :: Integer -> Integer -> Integer -> Integer
encryptExec e n m = powerMod m e n

decryptExec :: Integer -> Integer -> Integer -> Integer
decryptExec d n c = powerMod c d n

-- public functions
generateKeyPair = True
encrypt = True
decrypt = True



