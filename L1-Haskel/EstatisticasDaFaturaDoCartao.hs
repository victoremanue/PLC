clearData :: String -> [(Int, String, String, Double)]
clearData x = makeTuples splitString
      where makeTuples [] = []
            makeTuples m = (read (m!!0), m!!1, m!!2, read (m!!3)) : makeTuples (drop 4 m)
            splitString = words [if i==';' then ' ' else i | i <- x] 
                        --      map (\x -> case x of ';' -> ' '; _ -> x) x

minMaxCartao :: String -> (Double, Double)
minMaxCartao x = (minimum saldos, maximum saldos)
        where saldos = [d | (_,_,_,d) <- clearData x]

main = do
    a <- getLine
    let result = minMaxCartao a
    print result