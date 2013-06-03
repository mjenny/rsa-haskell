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