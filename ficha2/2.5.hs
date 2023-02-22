safetail :: Eq a => [a] -> [a]
safetail lista
          | lista == [] = []
          | otherwise = tail lista

safetail1 :: Eq a => [a] -> [a]
safetail1 lista = if lista == [] then [] else tail lista

safetail2 :: [a] -> [a]
safetail2 [] = []
safetail2 lista = tail lista
