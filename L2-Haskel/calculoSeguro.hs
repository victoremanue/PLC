import Prelude hiding (Maybe (..)) 
 
data Maybe a = Just a | 
               Nothing 
               deriving(Show) 
 
opr :: String -> String
opr [] = []
opr (a:as) | a >= 'a' && a <='z' =  take 3 (a:as)
           | otherwise = (opr as)

achaN :: String -> String 
achaN [] = []
achaN (a:as) | a >= '0' && a <= '9' =  a: (achaN as)
             | otherwise = [] 

converte :: Char -> Int 
converte x = fromEnum x - fromEnum '0' 
             
converteNum :: String -> Int -> Int
converteNum [] n = 0
converteNum (a:as) n = (converte a) * n + (converteNum as (n*10))

theCalc :: String -> Maybe Int      
theCalc str | opr str == "sum" = Just((converteNum (reverse(achaN str)) 1) + (converteNum (achaN(reverse str)) 1))
            | opr str == "sub" = Just((converteNum (reverse(achaN str)) 1) - (converteNum (achaN(reverse str)) 1))
            | opr str == "mul" = Just((converteNum (reverse(achaN str)) 1) * (converteNum (achaN(reverse str)) 1))
            | opr str == "div" && (converteNum (achaN(reverse str)) 1) /= 0 = Just((converteNum (reverse(achaN str)) 1) `div` (converteNum (achaN(reverse str)) 1))
            | otherwise = (Nothing)
 

safeCalc :: String -> IO () 
safeCalc x = print(theCalc x)