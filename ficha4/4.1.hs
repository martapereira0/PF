algarismos :: Int -> [Int]
algarismos n = reverse(algarismosRev n)

algarismosRev :: Int -> [Int]
algarismosRev 0 = []
algarismosRev n =  n`mod`10 : algarismosRev (n`div`10)
