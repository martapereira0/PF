fromBits :: [Int] -> Int
fromBits n = foldl (+) 0 [2^i | i<- [0..(length n)-1], ((reverse n) !! i) == 1 ]
