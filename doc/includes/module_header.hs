-- # Define RSA module and its public functions
module RSA
(  generatePrivateKey
,  generatePublicKey
,  encrypt
,  decrypt
) where

-- # import required external modules
import Prelude 		-- contains gcd function(greatest common divisor)
import System.Random
import Control.Monad.Fix 
import Data.Bits