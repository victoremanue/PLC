check :: String -> String -> Bool 
check [] [] = True
check (a:as) (b:bs) | a == b = (check as bs)
                    | otherwise = False

localizarGeral :: Int -> String -> String -> Int
localizarGeral n (a:as) [] = 0
localizarGeral n (a:as) (b:bs) | a == b && (check (a:as) (take (length (a:as)) (b:bs))) = n+1 
                               | otherwise = (localizarGeral (n+1) (a:as) bs)

localizar :: String -> String -> Int
localizar [] [] = 0
localizar (a:as) [] = 0
localizar [] (b:bs) = 0
localizar (a:as) (b:bs) = localizarGeral 0 (a:as) (b:bs)   