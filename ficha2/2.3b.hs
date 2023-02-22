max3,min3:: Ord a => a -> a -> a -> a
max3 a b c = max (max a b) c --1º argumento da função max é o max a b e o 2º é c
min3 a b c = min (min a b) c 
