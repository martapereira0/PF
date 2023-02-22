import Stack

calcular :: String -> Integer
calcular str = calculadoraaux (words str) empty

calculadoraaux :: [String] -> Stack Integer -> Integer
calculadoraaux [] stk = 0
calculadoraaux (y:ys) stk | y == "*"  = calculadoraaux ys (push (n1 + n2) rm)
                          | y == "+"  = calculadoraaux ys (push (n1 + n2) rm)
                          | y == "-"  = calculadoraaux ys (push (n1 - n2) rm)
                          | y == "/"  = calculadoraaux ys (push (div n1  n2) rm)
                          | otherwise = calculadoraaux ys (push (read x) stk)
            where
              n1 = top (pop stk)
              n2 = top stk
              rm = pop (pop stk)
