classifica :: Int -> String
classifica nota | nota <= 9 = ("reprovado")
                | nota <= 12 = ("suficiente")
                | nota <= 15 = ("bom")
                | nota <= 18 = ("muito bom")
                | otherwise = ("muito bom com distinção") 
