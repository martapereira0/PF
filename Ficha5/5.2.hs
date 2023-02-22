primo :: Integer -> Bool
primo n | n>1 && all (\i -> mod n i/=0) [2..floor(sqrt(fromIntegral n))] == True = True
        | otherwise = False
