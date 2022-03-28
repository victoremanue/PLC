data Tree t = Nilt |
               Node t (Tree t) (Tree t)
               deriving (Read, Show)

              
insert :: Ord t => Tree t -> t -> Tree t
insert  Nilt a  = (Node a Nilt Nilt)
insert (Node v esq dir) a | a < v = (Node v (insert esq a) dir)
                          | a > v = Node v esq (insert dir a)
                          | a == v = Node v esq dir 

insertList :: Ord t => Tree t -> [t] -> Tree t
insertList arvore [] = arvore
insertList arvore (a:as) = insertList (insert arvore a) as 

teste :: Tree Int
teste = Node 10 (Node 5 (Nilt) (Nilt)) (Nilt)

main = do
       a <- getLine
       b <- getLine
       let result = insertList (read a::Tree Int) (read b)
       print result