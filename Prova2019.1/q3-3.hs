data Comando = ParaFrente Int
             | ParaTras Int
             | Escreva Char
             deriving (Show, Eq)

insere:: String -> Char -> Int -> Int -> String
insere [] c x y = c:[]
insere (a:as) c x y | x > y = a:(insere as c x (y+1))
                    | otherwise = c:as

altera :: String -> [Comando] -> Int -> String
altera (a:as) [] n = (a:as)
altera (a:as) ((ParaFrente x):bs) n = (altera (a:as) (bs) (n+x))
altera (a:as) ((ParaTras x):bs) n = (altera (a:as) (bs) (n-x))
altera (a:as) ((Escreva y):bs) n = (altera (insere (a:as) y n 0) bs n)

estadofinal :: String -> [Comando] -> String
estadofinal (a:as) [] = (a:as)
estadofinal (a:as) (b:bs) = (altera (a:as) (b:bs) 0)