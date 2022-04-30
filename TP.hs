collatzAux :: Integer -> Integer -> Integer
collatzAux n r 
                |n==1 = r 
                |mod n  2==0 = collatzAux (div n 2) (r+1)
                |otherwise = collatzAux (3*n+1) (r+1)

satisfaceCollatz :: Integer -> Integer -> Bool
satisfaceCollatz n m 
                    |n==1 = True
                    |otherwise = collatzAux n 0 < m 

paresAux :: Integer -> Integer -> Integer
paresAux n r 
                |n==1 = r 
                |mod n  2 == 0 = paresAux (div n 2) (r+1)
                |otherwise = paresAux (3*n+1) (r)

cantidadTerminosPares :: Integer -> Integer
cantidadTerminosPares n = paresAux n 0

largoSecuencia :: Integer -> Integer
largoSecuencia n = collatzAux n 0


satisfaceCollatzHasta :: Integer -> Integer -> Bool
satisfaceCollatzHasta n m 
                          | n==1 = True
                          | satisfaceCollatz n m== True = satisfaceCollatzHasta (n-1) m 
                          | otherwise = False