type Comando = String
type Valor = Int

som :: String
som = "Soma"
mul :: String
mul = "Multiplica"
sub :: String
sub = "Subtrai"
di :: String
di = "Divide"

op:: Int -> [(String, Int)] -> Int
op x [] = x
op x ((a,b):abs) | a == som =  op (x+b) abs
op x ((a,b):abs) | a == sub =  op (x-b) abs
op x ((a,b):abs) | a == mul =  op (x*b) abs
op x ((a,b):abs) | a == di && b/=0 =  op (div x b) abs
op x ((a,b):abs) | a == di && b == 0 = -666

                  

executa :: [(Comando, Valor)] -> Int
executa [] = 0
executa ((a,b):(abs)) | a == som = op (0+b) abs
executa ((a,b):(abs)) | a == sub = op (0-b) abs
executa ((a,b):(abs)) | a == mul = op (0*b) abs
executa ((a,b):(abs)) | a == di = op 0 abs
                     
                    

