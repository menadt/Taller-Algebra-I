sumaDivisoresHasta :: Int -> Int -> Int
sumaDivisoresHasta s p
                        |p==1 = 1
                        |mod s p ==0 = p + sumaDivisoresHasta s (p-1)  
                        |otherwise = sumaDivisoresHasta s (p-1)  

sumaDivisores :: Int -> Int
sumaDivisores 1 = 1
sumaDivisores n = sumaDivisoresHasta n n

menorDivisorDesde :: Int -> Int -> Int
menorDivisorDesde n d
                    |mod n d==0 = d
                    |otherwise= menorDivisorDesde n (d+1)

menorDivisor :: Int -> Int
menorDivisor n = menorDivisorDesde n 2

esPrimo :: Int -> Bool
esPrimo 1 = False
esPrimo n = menorDivisor n==n

minimoPrimoDesde :: Int -> Int
minimoPrimoDesde 1 = 2
minimoPrimoDesde n
                    |esPrimo n = n
                    |otherwise = minimoPrimoDesde (n+1)

nEsimoPrimo  :: Int -> Int
nEsimoPrimo 1 = 2
nEsimoPrimo n = minimoPrimoDesde (1 + nEsimoPrimo (n-1))

factorial :: Int -> Int
factorial 0 = 1
factorial n = n * factorial (n-1)

menorFactDesdeD ::  Int -> Int -> Int
menorFactDesdeD n m
                    |factorial n >= m = factorial n
                    |otherwise = menorFactDesdeD (n+1) m 

menorFactDesde :: Int -> Int
menorFactDesde 1 = 1
menorFactDesde m = menorFactDesdeD 2 m

factorial2 :: Integral a => a -> a
factorial2 0 = 1
factorial2 n = n * factorial2 (n-1)

mayorFactHastaAux :: Integral a => a -> a -> a
mayorFactHastaAux m n
                    |m == 0 = 1
                    |factorial2 m > n = mayorFactHastaAux (m-1) n
                    |factorial2 m <= n = factorial2 m 
                    
mayorFactHasta :: Integral a => a -> a 
mayorFactHasta n = mayorFactHastaAux n n

esFactAux :: Integral a => a -> a  -> Bool
esFactAux n m
            |n == factorial2 m =True
            |n > factorial2 m = False
            |n < factorial2 m = esFactAux n (m-1)

esFact :: Integral a => a -> Bool
esFact 0 = False
esFact 1 = True
esFact n = esFactAux n n

fib :: Int-> Int
fib 0 = 1
fib 1 = 1
fib x = fib (x-1) + fib (x-2)

esFibAux :: Int -> Int -> Bool
esFibAux n m 
            |n == fib m =True
            |n > fib m = False
            |n < fib m = esFibAux n (m-1)

esFibonacci :: Int -> Bool
esFibonacci 0 = True
esFibonacci 1 = True
esFibonacci n = esFibAux n n

sumaPrimosAux :: Int -> Int 
sumaPrimosAux n 
                |n==1 = 1
                |esPrimo n = n + sumaPrimosAux (n-1)
                |esPrimo n== False = sumaPrimosAux (n-1)

sumaPrimosAuxDesde :: Int -> Int ->Bool
sumaPrimosAuxDesde n m
                        |m<2 = False
                        |m == 2 = True
                        |m == sumaPrimosAux n = True
                        |m > sumaPrimosAux n =  sumaPrimosAuxDesde (n+1) m 
                        |m< sumaPrimosAux n = False   

esSumaInicialDePrimos :: Int -> Bool
esSumaInicialDePrimos n = sumaPrimosAuxDesde 1 n
                        
valorMaxAux :: Int -> Int -> Int
valorMaxAux n1 

tomaValorMax :: Int -> Int -> Int
tomaValorMax n1 n2 
