data Tree t = Node t (Tree t) (Tree t) 
              | Nilt
              deriving (Read)


altura :: Tree t -> Tree t -> Int

alturaArvore :: Tree t -> Int
alturaArvore (Nilt) = 0
alturaArvore (Node x esq dir) = 1 + altura esq dir
                              
