strReturn :: String -> Int -> Int -> Int -> String
strReturn (a:as)  cur x y | cur < x = strReturn (a:as) (cur+1) x y
                          | cur >= x && y <= ((length (a:as)) - x) = take y (drop x (a:as))   
                          | otherwise = take ((length (a:as)) - x) (drop x (a:as))  


meio :: String -> Int -> Int -> String
meio [] x y = []
--meio (a:as) x y | x > length(a:as) = []
meio (a:as) x y | x >= 0 && y >= 0 = strReturn (a:as) 0 (x-1) y 
                | otherwise = []

