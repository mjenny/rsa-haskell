-- # Define RSA module and its public functions
module RSA
(  generateKeyPair
,  encrypt
,  decrypt
) where

-- # import required external modules
import Prelude 		-- contains gcd function(greatest common divisor)
import System.Random
import Control.Monad.Fix 
import Data.Bits


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
		      

euclid :: Integer -> Integer -> (Integer, Integer)
euclid 0 b = (0,1)
euclid a b
  | b == 0 = (1,0)
  | otherwise = (t, s-q*t)
    where
      (q, r) = quotRem a b
      (s, t) = euclid b r

inverseMod :: Integer -> Integer -> Integer
inverseMod a m =
  (x + m) `mod` m
  where
    (z, (x, y)) = ((gcd a m),euclid a m)

-- use to save performance while doing millerRabin
-- shiftR = bitwise right shift
--squareMultiply b e = 

-- public functions
generateKeyPair = True

encrypt k n m = mod (m^k) n

decrypt d n c = mod (c^d) n

