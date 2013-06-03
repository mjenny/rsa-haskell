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