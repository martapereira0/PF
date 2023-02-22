palavras :: String -> [String]
palavras s = words s-}

palavras s = case dropWhile isSpace s of
              "" -> []
              s' -> w : palavras s''
                where (w,s'') =
                    break isSpace s'
