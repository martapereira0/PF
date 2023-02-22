metades :: [a] -> ([a],[a])
metades l = (left,right)
  where
    left = take n l
    right = drop n l
    n = div (length l) 2
