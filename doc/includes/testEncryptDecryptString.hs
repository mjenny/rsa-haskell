testEncryptDecryptString :: String -> Integer -> Integer -> Integer -> Bool
testEncryptDecryptString m e d n = m == (decryptString d n (encryptString d n m))
