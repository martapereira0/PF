toBits :: Int -> [Int]
toBits n = reverse(invertedlist n)

invertedlist :: Int -> [Int]
invertedlist 0 = []
invertedlist n = n`mod`2 : invertedlist (n`div`2)
