curta :: Eq a => [a] -> Bool
curta a | (length a == 0 || length a == 1 || length a == 2) = True
        | otherwise = False

-- curtab :: Eq a => [a] -> Bool
-- curtab a  | (a == [] || a == [_] || a == [_,_]) = True
          -- | otherwise = False

curtab :: [a] -> Bool
curtab [] = True
curtab [_] = True
curtab [_,_] = True
curtab _ = False --listas com tamanho superior a 2
