mediana :: Int -> Int -> Int -> Int
mediana a b c
      | ((a>=b && b>=c) || (c>=b && b>=a)) = b
      | ((b>=a && a>=c) || (c>=a && a>=b)) = a
      | ((a>=c && c>=b) || (b>=c && c>=a)) = c


mediana1 :: Int -> Int -> Int -> Int
mediana1 a b c
      | (a>=b && b>=c) || (c>=b && b>=a) = soma - a - c
      | (b>=a && a>=c) || (c>=a && a>=b) = soma - b - c
      | (b>=c && c>=a) || (a>=c && c>=b) = soma - b - a
      where
        soma = a + b + c
