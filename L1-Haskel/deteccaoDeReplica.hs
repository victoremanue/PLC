isReplica :: String -> Int -> Char -> Bool
isReplica [] 0 c = True
isReplica [] n c = False
isReplica (a:as) 0 c = False
isReplica (a:as) n c | a == c = isReplica as (n-1) c
                     |otherwise = False
                     
main = do
    a <- getLine
    b <- getLine
    c <- getChar
    let result = isReplica a (read b) c
    print result