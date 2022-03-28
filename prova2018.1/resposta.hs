--Questão A --
data Time = Egito | Russia | Arabia | Uruguai | Ira | Marrocos | Portugal | Espanha deriving (Show, Eq)
type Grupo = (Char,[Time])
type Jogo = (Time, Int, Int, Time)

contagol :: Time -> Jogo -> Int
contagol tim (time1, g1, g2, time2) | tim == time1 = g1
                                    | tim == time2 = g2
                                    | otherwise = 0                     

gols :: Time -> [Jogo] -> Int
gols time [] = 0
gols time (a:as) = (contagol time a) + (gols time as) 

--Questão B--
contasaldo :: Time -> Jogo -> Int
contasaldo tim (time1, g1, g2, time2) | tim == time1 = g1-g2
                                      | tim == time2 = g2-g1
                                      | otherwise = 0  

saldo :: Time -> [Jogo] -> Int
saldo time [] = 0
saldo time (a:as) = (contasaldo time a) + (saldo time as) 

--Questão C--
contaponto :: Time -> Jogo -> Int
contaponto tim (time1, g1, g2, time2) | tim == time1 && g1 > g2 = 3
                                      | tim == time1 && g1 == g2 = 1
                                      | tim == time1 && g1 < g2 = 0
                                      | tim == time2 && g1 > g2 = 0
                                      | tim == time2 && g1 == g2 = 1
                                      | tim == time2 && g1 < g2 = 3
                                      | otherwise = 0  

pontos :: Time -> [Jogo] -> Int    
pontos time [] = 0
pontos time (a:as) = (contaponto time a) + (pontos time as)      

--Questão D--
gpA :: Grupo
gpA = ('A', [Egito, Russia, Arabia, Uruguai])
gpB :: Grupo
gpB = ('B', [Ira, Marrocos, Portugal, Espanha])

jogos1 :: [Jogo]
jogos1 = [(Egito, 1, 3, Russia), (Arabia, 0, 3, Uruguai), 
          (Egito, 0, 0, Arabia),(Russia, 0, 2, Uruguai), 
          (Russia, 2, 0, Arabia), (Egito, 0, 2, Uruguai), 
          (Ira, 1, 1, Marrocos), (Portugal, 2, 2, Espanha), 
          (Ira, 1, 2, Portugal), (Ira, 0, 1, Espanha), 
          (Marrocos, 0, 3, Portugal), (Marrocos, 1, 1, Espanha)]



primeiro :: Time -> [Time] -> [Jogo] -> Time
primeiro tim [] jogos = tim
primeiro tim (a:as) jogos | ((pontos tim jogos) > (pontos a jogos)) = (primeiro tim as jogos)
                          | ((pontos tim jogos) == (pontos a jogos)) &&
                            ((saldo tim jogos) > (saldo a jogos)) = (primeiro tim as jogos)
                          | ((pontos tim jogos) == (pontos a jogos)) && 
                            ((saldo tim jogos) == (saldo a jogos)) && 
                            ((gols tim jogos) > (gols a jogos)) = (primeiro tim as jogos)
                          | otherwise = (primeiro a as jogos) 

fTime:: Time -> [Time] -> Time
fTime tim (a:as) | a /= tim = a
                 |otherwise = (fTime tim as)  
                          
list:: Time -> [Time] -> [Time]
list tim [] = []
list tim (a:as) | a /= tim = a:(list tim as)
                |otherwise = (list tim as)
                        

classificados :: Grupo -> [Jogo] -> (Time, Time)
classificados (c,(a:as)) lista = ((primeiro a as lista), (primeiro (fTime (primeiro a as lista) (a:as)) (list (primeiro a as lista) (a:as)) lista) )
