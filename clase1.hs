doble :: Float -> Int
doble x = round (2 * x)

suma x y = x + y

normaVectorial v1 v2= sqrt (v1**2 + v2**2)

funcionConstante8 x = 8

f n | n == 0 = 1
    | n /= 0 =  0

g n | n == 0 = 1
    | otherwise = 0

signo n | n > 0 = 1
        | n == 0 = 0
        | n < 0 = -1

maximo x y | x >= y = x
           | otherwise = y

f3 n | n >= 3 = 5
     | n == 2 = undefined
     | otherwise = 8

ff 0 = 1
ff 5 = 2
ff n = 0

cantidadDeSoluciones b c | d > 0 = 2
                         | d == 0 = 1
                         | otherwise = 0
                         where d = b ^2 - 4* c

absoluto :: Int -> Int
absoluto 0 = 0
absoluto x | x >= 1 = x
           | x <= (-1) = (-x)


maximoAbsoluto :: Int -> Int -> Int
maximoAbsoluto x y | absoluto x > absoluto y = absoluto x
                   | absoluto x <= absoluto y = absoluto y

maximo3 :: Int -> Int -> Int -> Int
maximo3 x y z | (x >= y) && (x >= z) = x
              | (y >= x) && (y >= z) = y
              | (z >= x) && (z >= y) = z

algunoEs0 :: Float -> Float -> Bool
algunoEs0 x y | x == 0 = True
              | y == 0 = True
              | otherwise = False

algunoEs02 :: Float -> Float -> Bool
algunoEs02 0 y = True
algunoEs02 x 0 = True
algunoEs02 x y = False

ambosSon0 :: Float -> Float -> Bool
ambosSon0 x y | (x == 0) && (y == 0) = True
              | otherwise = False

ambosSon02 :: Float -> Float -> Bool
ambosSon02 0 0 = True
ambosSon02 n m = False

esMultiploDe :: Int -> Int -> Bool
esMultiploDe x y | mod x y == 0 = True
                 | otherwise = False

digitoUnidades :: Int -> Int
digitoUnidades x | x <= 10 = x
                 | otherwise = mod x 10

digitoDecenas :: Int -> Int
digitoDecenas x | x <= 100 = div x 10
                | otherwise = div (mod x 100) 10
