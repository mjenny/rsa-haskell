testPowerMod :: Integer -> Integer -> Integer -> Bool
testPowerMod b e m = powerMod b e m == mod (b^e) m