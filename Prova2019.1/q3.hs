data Comando = ParaFrente Int
             | ParaTras Int
             | Escreva Char
             deriving (Show, Eq)

cursor :: Comando -> Int 
cursor (ParaFrente n) = n
cursor (ParaTras n) = (-n)
cursor (Escreva x) = 0



posicaofinal :: String -> [Comando] -> Int
posicaofinal str [] = 1
posicaofinal str (a:as) = (cursor a) + (posicaofinal str as)