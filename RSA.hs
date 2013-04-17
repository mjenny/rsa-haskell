-- # Define RSA module and its public functions
module RSA
(  generatePrivateKey
,  generatePublicKey
,  encrypt
,  decrypt
) where

-- # import required external modules
import Prelude 		-- contains gcd function(greatest common decimal)

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

-- 
--getSmallOddInt

-- public functions
--generatePrivateKey
--generatePublicKey
encrypt k n M = mod (M^k) n

decrypt d n C = mod (C^d) n