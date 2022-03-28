data Comando = ParaFrente Int
             | ParaTras Int
             | Escreva Char
             deriving (Show, Eq)

insere:: String -> Char -> Int -> Int -> String
insere [] c x y = c:[]
insere (a:as) c x y | x > y = a:(insere as c x (y+1))
                    | otherwise = c:as


achaC :: String -> Int -> Int -> Char
achaC (a:as) x y | y == x = a
                 | otherwise = achaC (as) x (y+1)


altera :: String -> [Comando] -> Int -> Char
altera (a:as) [] n = (achaC (a:as) n 0)
altera (a:as) ((ParaFrente x):bs) n = (altera (a:as) (bs) (n+x))
altera (a:as) ((ParaTras x):bs) n = (altera (a:as) (bs) (n-x))
altera (a:as) ((Escreva y):bs) n = (altera (insere (a:as) y n 0) bs n)

interprete :: String -> [Comando] -> Char
interprete (a:as) [] = a
interprete (a:as) (b:bs) = (altera (a:as) (b:bs) 0)