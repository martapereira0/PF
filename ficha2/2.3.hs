max3 :: Ord a => a -> a -> a -> a
max3 a b c  | ((a>b && a>c) || (a>=b && a>c) || (a>b && a>=c)) = a
            | ((b>c && b>a) || (b>=a && b>c) || (b>a && b>=c)) = b
            | ((c>a && c>b) || (c>=b && c>a) || (c>b && c>=a)) = c
            | (c==a && b==a) = a

min3 :: Ord a => a -> a -> a -> a
min3 a b c  | ((a<b && a<c) || (a<=b && a<c) || (a<b && a<=c))= a
            | (b<c && b<a) || (b<=a && b<c) || (b<a && b<=c)) = b
            | (c<a && c<b) || (c<=b && c<a) || (c<b && c<=a))= c
            | (c==a && b==a) = a
