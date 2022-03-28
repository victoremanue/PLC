--Questão 1--
encapsula :: Int -> Char-> String -> String
encapsula n cr [] = ((show(n))++cr:[])
encapsula n cr (a:as) | cr == a = (encapsula (n+1) cr as)
                      | otherwise = ((show(n))++cr:[])
                      
verifica:: Char -> String -> String
verifica x [] = []
verifica x (a:as) | x == a = verifica x as
                  | otherwise = (a:as)

encode_rle :: String -> String
encode_rle [] = []
encode_rle (a:as) = (encapsula 1 a as) ++ (encode_rle (verifica a as)) 

--Questão 2-
charToInt :: Char -> Int
charToInt ch = fromEnum ch - fromEnum '0'

dec :: Int -> Char -> String
dec rep carac | 1 < rep = carac:(dec (rep-1) carac)
              | otherwise = (carac:[])

decode_rle :: String -> String
decode_rle [] = []
decode_rle (a:as) | (a >= '1') && (a <= '9') = (dec (charToInt a) (head as)) ++ (decode_rle as )
                  | otherwise = (decode_rle as)

--Questão 3--
type Dicionario = [(Int, String)]

meuDicionario :: Dicionario
meuDicionario = [(1, "casa"), (3, "cafe"), (4, "teria"), (6, "era"), (7, "uma")]

teste :: String
teste = "a 1 tinha 3 mas nao 6 7 34, a 1 6 7 sorve4"

substitui :: Int -> Dicionario -> String
substitui n [] = (show n)
substitui n ((x,y):xys) | n == x = y
                        | otherwise = substitui n xys 

decode :: Dicionario -> String -> String 
decode [] str = str
decode dic [] = []
decode dic (x:xs) | x >= '0' && x<= '9' = (substitui (charToInt x) dic) ++ (decode dic xs)
                  | otherwise = x:(decode dic xs)

--Questão 4--
type DicionarioT = Tree Int String
data Tree chave valor = Node chave valor (Tree chave valor) (Tree chave valor)
                      | Leaf

meuDicionarioT :: DicionarioT
meuDicionarioT = Node 4 "teria" (Node 3 "cafe" (Node 1 "casa" Leaf Leaf) Leaf)
                                (Node 6 "era" Leaf (Node 7 "uma" Leaf Leaf))                      

busca :: DicionarioT -> Int -> String
busca (Leaf) n = (show n)
busca (Node x str esq  dir) n | n == x = str
busca (Node x str esq  dir) n | n < x = (busca esq n)
busca (Node x str esq  dir) n | n > x = (busca dir n)


decodeTree :: DicionarioT -> String -> String
decodeTree (Leaf) str = str
decodeTree arvore [] = []
decodeTree arvore (a:as) | a >= '0' && a <= '9' = (busca arvore (charToInt a)) ++ (decodeTree arvore as)
                         | otherwise = a:(decodeTree arvore as)