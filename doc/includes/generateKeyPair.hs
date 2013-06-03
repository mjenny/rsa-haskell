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