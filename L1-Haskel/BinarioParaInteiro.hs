tamanho :: String -> Int
tamanho [] = 0
tamanho (a:as) = 1 + tamanho as

potencia:: Int -> Int
potencia 0 = 1 
potencia 1 = 2
potencia n = 2 * potencia (n-1) 

btoi :: String -> Int
btoi [] = 0
btoi (a:as) | a == '0' = 0 + btoi(as)
btoi (a:as) | a == '1' = potencia(tamanho(a:as)-1) + btoi(as)

main = do
    s <- getLine
    let result = btoi s
    print result