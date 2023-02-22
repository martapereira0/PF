aux :: Int -> [Int]
aux n = [2^i*3^j*5^k | i <- [0..] , j <-[0..] , k <- [0..], i+j+k==nghci
]
