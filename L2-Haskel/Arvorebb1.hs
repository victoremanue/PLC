data Tree t = Nilt |
              Node t (Tree t) (Tree t)
              deriving (Read)

nEsq :: Ord t => t -> Tree t -> Bool
nEsq x (Nilt) = True
nEsq x (Node n esq dir) | x >= n && (nEsq x esq) && (nEsq x dir) = True
                        |otherwise = False

nDir :: Ord t => t -> Tree t -> Bool
nDir x (Nilt) = True
nDir x (Node n esq dir) | x < n && (nDir x esq) && (nDir x dir)  = True
                        |otherwise = False

isBST :: Ord t => Tree t -> Bool
isBST (Nilt) = True
isBST (Node x esq dir) |(nEsq x esq) && (nDir x dir) = isBST esq && isBST dir
                       |otherwise = False

