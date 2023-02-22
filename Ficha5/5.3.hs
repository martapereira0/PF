-- esconder a definição do prelúdio
import Prelude hiding ((++))

-- completar esta definição
(++) :: [a] -> [a] -> [a]
xs ++ ys = foldr f z lista
    where f     = (:)
          z     = ys
          lista = xs

concat1 :: [[a]] -> [a]
concat1 xss = foldr f z lista
  where
   f = (++)
   z = []
   lista = xss

reverse1 :: [a] -> [a]
reverse1 (x:xs) = reverse1 xs ++ foldr f z x
  where
    f = (:)
    z = []
