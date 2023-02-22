import Stack

parent :: [Char] -> Bool
parent str = parentAux str empty

parentAux :: [Char] -> Stack Char -> Bool
parentAux [] stk = isEmpty stk
parentAux (x:xs) stk
    | x == '(' = parentAux xs (push '(' stk)
    | x == ')' = not (isEmpty stk) && top stk == '(' && parentAux xs (pop stk)
    | x == '[' = parentAux xs (push '[' stk)
    | x == ']' = not (isEmpty stk) && top stk == '[' && parentAux xs (pop stk)
    | x == '{' = parentAux xs (push '{' stk)
    | x == '}' = not (isEmpty stk) && top stk == '{' && parentAux xs (pop stk)
    | otherwise = parentAux xs stk
