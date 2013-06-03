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
