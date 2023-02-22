
nub :: (Eq a) => [a] -> [a]
nub []             =  [] --string vazia
nub (x:xs) = x : nub (naorep x xs)
  where
    naorep  n 1= [y| y <- 1, y/=x  ]
