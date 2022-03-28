--Questão 1--
primeira :: Ord t => Int -> t -> [t] -> Int
primeira n cr [] = 0
primeira n cr (a:as) | cr == a = n
                     | otherwise = (primeira (n+1) cr as)
findVall:: Ord t => t -> [t] -> Int
findVall cr (a:as) = (primeira 1 cr (a:as)) 

-- Questao 2 --
verifica :: String -> String -> Bool
verifica [] [] = True
verifica (a:as) [] = False
verifica (a:as) (b:bs) | a == b = verifica as bs
                       |otherwise = False 

substr :: String -> String -> Bool
substr [] str2 = False
substr str [] = False
substr (a:as) (b:bs) | a == b && verifica (a:as) (take (length (a:as)) (b:bs)) = True 
                     | otherwise = (substr (a:as) bs)

--Questão 3--
data Result = NotInWord | WrongPos | OK
              deriving Show
ver :: Char -> String -> Int -> Int -> Result
ver cr [] oc pA= (NotInWord)
ver cr (a:as) oc pA | cr == a && oc == pA = (OK)
                    | cr == a && oc /= pA = (WrongPos)
                    | otherwise = (ver cr as oc (pA+1))

tryWord :: String -> String -> [Result]
tryWord [] (b:bs) = []
tryWord (a:as) (b:bs) = (ver a (b:bs) ((length (b:bs)) - (length (a:as))) 0):tryWord as (b:bs)

--Questão 4 --
data Tree t = Node t (Tree t) (Tree t) | Leaf t 
            deriving (Read, Show)
          
busca :: String -> Tree String -> Bool
busca palavra (Leaf []) = False
busca palavra  (Leaf cat) = palavra == cat
busca palavra (Node str esq dir) | palavra == str = True
                                 | palavra < str = busca palavra esq
                                 | palavra > str = busca palavra dir

arv2:: Tree String
arv2 = Node "PORTA" (Leaf "MESA") (Leaf "TELHADO")
arv1 = Node "CASA" (Node "ARVORE" (Leaf "AMARELO") (Leaf "AZUL")) (Node "LIVRO" (Leaf "JANELA") arv2)