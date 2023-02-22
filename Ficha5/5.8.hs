scanl' :: (a -> b -> a) -> a -> [b] -> [a]
scanl' f z [] = []
scanl' f z (x:xs) = f z x : scanl' f y xs
          where
            y=z+x
