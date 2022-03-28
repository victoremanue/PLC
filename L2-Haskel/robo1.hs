data Command = Forward Int | Backward Int | TurnLeft | TurnRight 
               deriving (Eq, Show, Read)


modificay :: (Int, Int) -> Int -> (Int, Int)
modificay (x, y) n = (x, (y+n))

modificax :: (Int, Int) -> Int -> (Int, Int)
modificax (x, y) n = ((x+n), y)

decidexy :: (Int, Int) -> Char -> [Command] -> (Int, Int)
decidexy (x,y) eixo [] = (x,y)
decidexy (x, y) 'n' ((Forward n):as) = decidexy (modificay (x,y) n) 'n' as
decidexy (x, y) 'n' ((Backward n):as) = decidexy (modificay (x,y) (-n)) 'n' as 
decidexy (x, y) 'n' ((TurnLeft):as) = decidexy (x,y) 'o' as  
decidexy (x, y) 'n' ((TurnRight):as) = decidexy (x,y) 'l' as 
decidexy (x, y) 's' ((Forward n):as) = decidexy (modificay (x,y) (-n)) 's' as
decidexy (x, y) 's' ((Backward n):as) = decidexy (modificay (x,y) n) 's' as 
decidexy (x, y) 's' ((TurnLeft):as) = decidexy (x,y) 'l' as  
decidexy (x, y) 's' ((TurnRight):as) = decidexy (x,y) 'o' as
decidexy (x, y) 'o' ((Forward n):as) = decidexy (modificax (x,y) (-n)) 'o' as
decidexy (x, y) 'o' ((Backward n):as) = decidexy (modificax (x,y) n) 'o' as 
decidexy (x, y) 'o' ((TurnLeft):as) = decidexy (x,y) 's' as  
decidexy (x, y) 'o' ((TurnRight):as) = decidexy (x,y) 'n' as   
decidexy (x, y) 'l' ((Forward n):as) = decidexy (modificax (x,y) n) 'l' as
decidexy (x, y) 'l' ((Backward n):as) = decidexy (modificax (x,y) (-n)) 'l' as 
decidexy (x, y) 'l' ((TurnLeft):as) = decidexy (x,y) 'n' as  
decidexy (x, y) 'l' ((TurnRight):as) = decidexy (x,y) 's' as   
  

            
destination :: (Int,Int) -> [Command] -> (Int,Int) 
destination (x,y) (a:as) = decidexy (x,y) 'n' (a:as)