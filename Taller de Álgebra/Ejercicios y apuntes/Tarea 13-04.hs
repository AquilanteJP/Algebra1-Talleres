f2entero :: Int -> Int -> Int --sumatoria desde i=1 hasta n de q^i, q constante y entero
f2entero n q | n == 1 = q
       | otherwise = q^n + f2entero (n-1) q

f2recip :: Int -> Int -> Int --sumatoria desde i=1 hasta n de i^q, q constante y entero
f2recip n q | n == 1 = 1
            |otherwise = n^q + f2recip (n-1) q

gLoca :: Int -> Int -> Int
gLoca i n | n == i = fromIntegral(n^n)
          | otherwise = gLoca i (n-1) + fromIntegral(f2recip (n-1) n) - fromIntegral(f2recip (i-1) n) + f2entero n n - f2entero (i-1) n

g1 :: Int -> Int -> Int 
g1 i n | n == i = n^n
       | otherwise = g1 i (n-1) + i^n

g2 :: Int -> Int 
g2 n | n == 1 = g1 1 1
     | otherwise = g2 (n-1) + f2recip (n-1) n + f2entero n n

--sumaDigIguales :: Int -> Int 
--sumaDigIguales n | n == 1 = 1
                