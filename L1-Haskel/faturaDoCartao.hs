clearData :: String -> [(Int, String, String, Double)]
clearData x = makeTuples splitString
      where makeTuples [] = []
            makeTuples m = (read (m!!0), m!!1, m!!2, read (m!!3)) : makeTuples (drop 4 m)
            splitString = words [if i==';' then ' ' else i | i <- x] 
                        --      map (\x -> case x of ';' -> ' '; _ -> x) x

logMes :: String -> String -> Double
logMes mes x = foldl (+) 0 [d | (_,b,_,d) <- clearData x, b == mes]

main = do
    a <- getLine
    b <- getLine
    let result = logMes a b
    print result