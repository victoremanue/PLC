aux:: Char-> [(Char,Char)] -> Char
aux x [] = x
aux x ((j,k):jks)
    | x == j = k
    | x /= j = (aux x jks)

decEnigma :: String -> [(Char, Char)] -> String
decEnigma [] [] = []
decEnigma [] _ = []
decEnigma _ [] = []
decEnigma (x:xs) ((a,b): abs) 
    | x == a = b : decEnigma xs ((a,b): abs)
    | x /= a = (aux x ((a,b): abs)) : (decEnigma xs ((a,b): abs))
       
main = do
    a <- getLine
    b <- getLine
    let result = decEnigma a (read b)
    print result