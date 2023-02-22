divisores :: Int -> [Int]
divisores n = filter(\x->x'mod'n==0) [1..n]
