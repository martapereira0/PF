xor :: Bool -> Bool -> Bool
xor a b | a==b = False
        | otherwise = True

xor2 :: Bool -> Bool -> Bool
xor2  False False = False
xor2  False True = True
xor2 True False = True
xor2 True True = False
