data Command = Forward Int | Backward Int | TurnLeft | TurnRight 
               deriving (Eq, Show, Read)

data Direction = North | South | West | East
                 deriving (Read, Show)

dirFinal :: Command -> Direction -> Direction
dirFinal TurnLeft North = West 
dirFinal TurnLeft South = East 
dirFinal TurnLeft West = South 
dirFinal TurnLeft East  = North 
dirFinal TurnRight North = East  
dirFinal TurnRight South = West
dirFinal TurnRight West = North 
dirFinal TurnRight East = South 
dirFinal _ dirAt  = dirAt 
                        

faces :: Direction -> [Command] -> Direction
faces x [] = x
faces x (a:as) = faces (dirFinal a x) as