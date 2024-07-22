sumatoria :: Int -> Int
sumatoria 1 = 1
sumatoria n = n + sumatoria (n-1)

f1 :: Int -> Int
f1 0 = 1
f1 n = 2^n + f1 (n-1)

f2 :: Int -> Float -> Float 
f2 n q | n == 1 = q
       | otherwise = q^n + f2 (n-1) q
       
f3 :: Int -> Float -> Float      
f3 0 q = 0
f3 n q = q^(2*n) + q^(2*n-1) + f3 (n-1) q    --la suma va hasta 2n, entonces para que me funque el n-1 debo incluir el paso 2*n-1 (para n=4, q^8 + q^7 + la recursion, que va a ser q^6 + q^5, hasta llegar a q^0=1)

f4 :: Int -> Float -> Float
f4 n q | n == 0 = 1
       | otherwise = q^(2*n) + q^(2*n-1) - q^(n-1) + f4 (n-1) q  --si yo tengo n = 4, 2n=8, estoy restando q^3, q^2, q^1 y q^0, lo que me devuelve la suma de f3 pero empezando desde n = 4, que es lo que quiero

fact :: Int -> Int
fact 0 = 1
fact n = n * fact(n-1)
       
eAprox :: Int -> Float
eAprox n | n == 0 = 1
         | otherwise = (1.0)/(fromIntegral(fact n)) + eAprox (n-1) --si uso fact, que devuelve solo integers, debo usar el fromIntegral para convertirla en Float?, xq debo usar / si o si y no div

e :: Float
e = eAprox 10 

sumDoble :: Int -> Int -> Float   --plantear doble sumatoria como matriz
sumDoble 0 m = 0
sumDoble n m = f2 m (fromIntegral n) + sumDoble (n-1) m 

sumPot :: Float -> Int -> Int -> Float
sumPot q n m | m == 1 = q*(f2 n q)
             | otherwise = sumPot q n (m-1) + (q^m)*(f2 n q)
             
sumRac :: Int -> Int -> Float
sumRac n 1 = fromIntegral(sumatoria n)
sumRac n m = (1/(fromIntegral m))*(fromIntegral(sumatoria n)) + sumRac n (m-1)             
