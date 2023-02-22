binom' :: Integer -> Integer -> Integer
binom' n k |k < n - k = div (product[(n-k+1)..n]) (product[1..k])
           |otherwise = div (product[(k+1)..n]) (product[1..(n-k)])
      where
        j = n -k
        r = j +1
        s = k+1
