{- incr, triplo :: Integer -> Integer
incr x = x+1
triplo x = 3*x
boasVindas :: String -> String
boasVindas nome = "Olá, " ++ nome ++ "!" -}


--1.1)
--incr (triplo 3) = incr (3*3) = incr (9) = 9+1 = 10
--triplo (incr 3) = triplo (4) = 4*3 = 12
--boasVindas "Linguagem" ++ "Haskell" = Olá, Linguagem! Haskell
--boasVindas ("Linguagem" ++ "Haskell") = Olá, LinguagemHaskell!
--boasVindas (boasVindas "Haskell") = boasVindas(Olá, Haskell!) = Olá, Olá, Haskell!!

--1.2)
testaTriangulo :: Float -> Float -> Float -> Bool
testaTriangulo a b c  | a < b + c && b < a + c && c < b + a = True
                      | otherwise = False

--1.3)
areaTriangulo :: Float -> Float -> Float -> Float
areaTriangulo a b c = sqrt (s*(s-a)*(s-b)*(s-c))
      where
        s = (a+b+c)/2

--1.4)
metades :: [a] -> ([a],[a])
metades l = (esq,dir)
    where
      divi = div (length l) 2
      esq = take divi l
      dir = drop divi l
--1.5)
last1 :: [a] -> a
last1 l = head (reverse l)

init1 :: [a] -> [a]
init1 l = reverse (tail (reverse l))

init2 :: [a] -> [a]
init2 l = (reverse (drop 1 (reverse l)))

--1.6)
binom :: Integer -> Integer -> Integer
binom n k = product [1..n]`div`(product [1..k] * product [1..(n-k)])

binom' :: Integer -> Integer -> Integer
binom' n k  | k < n-k = product [(n-k+1)..n]`div`product [1..k]
            | k >= n-k = product [(k+1)..n]`div`product [1..(n-k)]
