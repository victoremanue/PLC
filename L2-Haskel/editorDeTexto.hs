data Cmd = Cursor Int
           | Backspace Int
           | Delete Int
           | Insert String
           deriving (Read)

delete :: Int -> Int -> Int -> String -> String
delete  c pA n (a:as) | c > pA = a: delete c (pA+1) n as
                      | otherwise = drop n (a:as)

inserir:: Int -> Int -> String -> String -> String
inserir c pA str [] = str
inserir c pA str (a:as) | c > pA = a: inserir c (pA+1) str as
                        |otherwise = str ++ (a:as)

editor :: Int -> String -> [Cmd] -> String
editor cur string [] = string
editor cur string ((Cursor n):bs) = editor (n+cur) string bs
editor cur string ((Delete n):bs) = editor cur (delete cur 0 n string) bs
editor cur string ((Backspace n):bs) = editor (cur-n) (reverse(delete (length string - cur) 0 n (reverse string))) bs
editor cur string ((Insert str):bs) = editor cur (inserir cur 0 str string) bs


editText :: String -> [Cmd] -> String
editText [] [] = []
editText (a:as) [] = (a:as)
editText [] (b:bs) = editor 0 [] (b:bs)
editText (a:as) (b:bs) = editor 0 (a:as) (b:bs)

main = do
       a <- getLine
       b <- getLine
       let result = editText a (read b)
       print result