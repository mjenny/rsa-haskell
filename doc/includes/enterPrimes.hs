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
	-- ignore isPrime to use large possible primes generated with sage (http://www.sagemath.org)
        --if ((gcd e phi) == 1 && (isPrime p) && (isPrime q)) then
        if ((gcd e phi) == 1) then
           return (p, q)
        else
           again
