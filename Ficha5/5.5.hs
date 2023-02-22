zipWith1 :: (a->b->c) -> [a] -> [b] -> [c]
zipWith1 _ [] [] = []
zipWith1 _ [] ys = [ys]
zipWith1 _ xs [] = [xs]
zipWith1 (f) (x:xs) (y:ys) = (f) x y : zipWith1 (f) xs ys
