f1 :: Int -> Int
f1 0 = 1
f1 n = 2^(n) + f1 (n-1)

f2 :: Int -> Float -> Float
f2 n q | n==0 = 0
    | otherwise = q ^ (n) + f2 (n-1) q 

f3 :: Int -> Float -> Float
f3 0 q = 0
f3 n q = q ^ (2*n) + q ^ (2*n-1) + f3 (n-1) q 

f4 :: Int -> Float -> Float
f4 0 q = 1
f4 n q = (f3 n q) - (f2 (n-1) q)

factorial :: Int -> Int
factorial 0 = 1
factorial n = n * factorial (n-1)

sumatoria :: Int -> Int
sumatoria 0 = 0
sumatoria n = sumatoria (n-1) + n

eAprox :: Int -> Float
eAprox 0 = 1
eAprox n = (eAprox (n-1)) + 1 / (fromIntegral (factorial n))

e :: Float
e = eAprox 10

f5 :: Int -> Int -> Int
f5 0 m = 0
f5 n m = (f5 (n-1) m )+ round (f2 m (fromIntegral(n))) 

sumaPotencias :: Float -> Int -> Int -> Float
sumaPotencias q n 0 = 0
sumaPotencias q n m = (sumaPotencias q n (m-1)) + (q ^ m) * (f2 n q)

sumaRacionales :: Int -> Int -> Float
sumaRacionales n 0 = 0
sumaRacionales n m = sumaRacionales n (m-1) + fromIntegral((sumatoria n)) / fromIntegral (m) 

g1 :: Int -> Int -> Int
g1 i n
        | n >= i = g1 i (n-1) + i ^ n
        | n < i = 0

g2 :: Int -> Int
g2 0 = 0
g2 n = g2 (n-1) + g2Aux n n

g2Aux :: Int -> Int -> Int
g2Aux 0 n = 0
g2Aux b n = b^n + g2Aux (b-1) n

g3 :: Int -> Int
g3 n 
    |n ==0 = 0
    |mod n 2 ==0 = g3 (n-2) + 2 ^ n 
    |mod n 2 /= 0 = g3 (n-1)

digitosIguales :: Int -> Bool
digitosIguales n 
                 |n < 0 = digitosIguales (-n)
                 |n < 10 = True
                 |mod n 10 == mod (div n 10) 10 = digitosIguales (div n 10)
                 |otherwise = False

g4 :: Int -> Int
g4 n
    |n==0 = 0
    |digitosIguales n = n + g4 (n-1)
    |otherwise = g4 (n-1)


