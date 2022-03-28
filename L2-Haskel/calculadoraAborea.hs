data Ops = SUM | MUL | SUB
           deriving (Read)

data IntTree = Nilt Int |
               Node Ops IntTree IntTree
               deriving (Read)

evalTree :: IntTree -> Int
evalTree (Nilt x) = x
evalTree (Node SUM esq dir) = evalTree esq + evalTree dir
evalTree (Node MUL esq dir) = evalTree esq * evalTree dir
evalTree (Node SUB esq dir) = evalTree esq - evalTree dir
