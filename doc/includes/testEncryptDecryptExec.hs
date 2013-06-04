testEncryptExec :: Integer -> Integer -> Integer -> Integer -> Bool
testEncryptDecryptExec e n m c = (encryptExec e n m) == c

testDecryptExec :: Integer -> Integer -> Integer -> Integer -> Bool
testDecryptExec d n m c = (decryptExec d n c) == m
