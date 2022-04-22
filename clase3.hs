fib :: Int -> Int
fib 0 = 1
fib 1 = 1
fib x = fib (x-1) + fib (x-2)


multiplo3 :: Int -> Bool
multiplo3 x| x==0 = True
           | x==1 = False
           | x==2 = False
           | otherwise= multiplo3 (x-3)

sumaImpares :: Int -> Int
sumaImpares 0 = 0
sumaImpares 1 = 1
sumaImpares n = 2*n-1 + sumaImpares (n-1)

medioFact :: Int -> Int
medioFact 1 = 1
medioFact 2 = 2
medioFact x = x*medioFact(x-2)

sumaDigitos :: Int -> Int
sumaDigitos n | div n 10 == 0 = n
              | otherwise = (mod n 10) + sumaDigitos (div n 10)

digitosIguales :: Int -> Bool
digitosIguales n 
                 |n < 0 = digitosIguales (-n)
                 |n < 10 = True
                 |mod n 10 == mod (div n 10) 10 = digitosIguales (div n 10)
                 |otherwise = False

parteEntera :: Float -> Int
parteEntera n
             |n<1 && n>0 = 0
             |otherwise = 1 + parteEntera (n-1)
             