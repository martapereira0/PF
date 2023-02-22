--2.1)
classifica' :: Int -> String
classifica' n  | n <= 9 = "reprovado"
              | n <= 12 = "suficiente"
              | n <= 15 = "bom"
              | n <= 18 = "muito bom"
              | otherwise  = " muito bom com distinção"

classifica1 :: Int -> String
classifica1 n = if n <= 9 then "reprovado" else if n <= 12 then "suficiente" else if n <= 15 then "bom" else if n <= 18 then "muito bom" else "muito bom com distinção"

--2.2)
classifica :: Float -> Float -> String
classifica peso alt | imc < 18.5 = "baixo peso"
                    | imc < 25 = "peso normal"
                    | imc < 30 = "excesso de peso"
                    | otherwise = "obesidade"
    where
      imc = peso/alt^2

--2.3)
--a)
max3 , min3 :: Ord a => a -> a -> a -> a
max3 x y z  | (x >= y && x >= z) || (y >= z && x >= y) = x
            | (x >= y && z >= x) || (y >= x && z >= y) = z
            | (x >= z && y >= z) || (z >= x && y >= z) = y
min3 x y z | (x <= y && x <= z) || (y <= z && x <= y) = x
            | (x <= y && z <= x) || (y <= x && z <= y) = z
            | (x <= z && y <= z) || (z <= x && y <= z) = y

--b)
max2 , min2 :: Ord a => a -> a -> a
max2 x y = if x>=y then x else y
min2 x y = if x<=y then x else y

max3' , min3' :: Ord a => a -> a -> a -> a
max3' x y z = if max x y >= z then max x y else z
min3' x y z = if max x y <= z then max x y else z
      where
        max x y = max2 x y

--2.4)
xor :: Bool -> Bool -> Bool
xor False False = False
xor True False = True
xor False True = True
xor True True = False

--2.5)
safetail1 :: Eq a => [a] -> [a]
safetail1 l | l == [] = []
            | otherwise = tail l

safetail2 :: Eq a => [a] -> [a]
safetail2 l = if l == [] then [] else tail l

safetail3 :: Eq a => [a] -> [a]
safetail3 [] = []
safetail3 l = tail l


--2.6)
curta :: Eq a => [a] -> Bool
curta l | (l == [] || length l == 1 || length l == 2) = True
        | otherwise = False

curta' :: Eq a => [a] -> Bool
curta' [] = True
curta' [x] = True
curta' [x,y] = True
curta' _ = False

--2.7)
--a) o tipo mais geral é Integer
-- comparações de ordem são comparações com > < =
mediana :: Int -> Int -> Int -> Int
mediana a b c | (c >= a && a >= b) || (a >= c && b >= a) = a
              | (a >= b && b >= c) || (b >= a && c >= b) = b
              | (c >= b && a >= c) || (c >= a && b >= c) = c
--b)
mediana' :: Int -> Int -> Int -> Int
mediana' x y z  | (max3' x y z == x && min3' x y z == y) || (max3' x y z == y && min3' x y z == x) = soma-x-y
                | (max3' x y z == x && min3' x y z == z) || (max3' x y z == z && min3' x y z == x) = soma-x-z
                | (max3' x y z == y && min3' x y z == z) || (max3' x y z == z && min3' x y z == y) = soma-x-y
    where
      soma = x + y + z
--2.8)

unidades :: [String]
unidades = ["zero","um","dois","três","quatro","cinco","seis","sete","oito","nove"]

dez_a_dezanove :: [String]
dez_a_dezanove = ["dez","onze","doze","treze","quatorze","quinze","dezasseis","dezassete","dezoito","dezanove"]

dezenas :: [String]
dezenas = ["vinte","trinta","quarenta","cinquenta","sessenta","setenta","oitenta","noventa"]

{- A função 'converte2' é composição de duas,
'divide2' obtêm os algarismos e 'combina2' combina o texto de cada algarismo.
Vamos usar as operações de concatenação (++) e indexação de listas (!!), os índices começam em zero.
-}

converte2 :: Int -> String
converte2 n | n<100 = combina2 (divide2 n)

divide2 :: Int -> (Int,Int)
divide2 n = (n`div`10,n`mod`10) --(quociente,resto)

combina2 :: (Int,Int) -> String
combina2 (0,u) = unidades !! u
combina2 (1,u) = dez_a_dezanove !! u
combina2 (d,0) = dezenas !! (d-2)
combina2 (d,u) = dezenas !! (d-2) ++ " e " ++ unidades !! u

centenas :: [String]
centenas = ["cento","duzentos","trezentos","quatrocentos","quinhentos","seiscentos","setecentos","oitocentos","novecentos"]

converte3 :: Int -> (Int,Int)
converte3 n | n<100 = combina3 (divide3 n)

divide3 :: Int -> (Int,Int)
divide3 n | n<100 = combina3 (divide3)

combina3 :: (Int,Int) -> String
combina3 (0,n) = converte2 n
combina3 (1,0) = "cem"
combina3 (c,0) = centenas !! (c-1)
combina3 (c,n) = centenas !! (c-1) ++ " e " ++ converte2 n

divide6 n = (n`div`1000, n`mod`1000)

combina6 (0,n) = converte3 n
combina6 (0, n) = converte3 n
combina6 (1, 0) = "mil"
combina6 (1, n) = "mil" ++ ligar n ++ converte3 n
combina6 (m, 0) = converte3 m ++ " mil"
combina6 (m, n) = converte3 m ++ " mil" ++ ligar n ++ converte3 n

{- Uma função auxiliar para escolher a partícula de ligação entre
   milhares e o restante (r).
   Regra: colocamos "e" quando o resto é inferior a 100
   ou múltiplo de 100; caso contrario, basta um espaço.
 -}
ligar :: Int -> String
ligar r
  | r < 100 || r `mod` 100 == 0 = " e "
  | otherwise                   = " "

-- A solução do exercício proposto é converte6.
converte :: Int -> String
converte = converte6


converte :: Int -> String
converte n
