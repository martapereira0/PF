ult :: [a] -> a
ult l = head (reverse l)
{-
ult :: [a] -> a
ult l = head ( drop (length l-1) l)
-}

