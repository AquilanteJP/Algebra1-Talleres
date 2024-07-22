factorial :: Int -> Int
factorial n | n == 0 = 1
            | n > 0 = n * factorial (n-1)
            
factorialCheto :: Int -> Int
factorialCheto n | n == 0 = 1
                 | otherwise = n * factorial (n-1)           

--con pattern matching
factorialZarpado :: Int -> Int
factorialZarpado 0 = 1
factorialZarpado n = n * factorial (n-1)    

esPar :: Int -> Bool
esPar n | n == 0 = True
        | n == 1 = False
        | otherwise = esPar (n-2)  
        
fib :: Int -> Int
fib n | n == 0 = 0
      | n == 1 = 1
      | otherwise = fib (n-1) + fib (n-2)

--si x es mayor que 1, le aplica la funcion a x-1 y le agrega +1 en cada paso; eventualmente, llega a x < 1 y te quedan todos los +1 sumados (a 4.3 se le resto 1 4 veces para llegar a 0.3, entonces devuelve 4)
--el +1 sirvió como contador    
parteEntera :: Float -> Int
parteEntera x | x < 1 = 0
              | otherwise = parteEntera (x-1) + 1
              
multDeTres :: Int -> Bool
multDeTres n | n == 3 = True
             | n < 3 = False
             | otherwise = multDeTres (n-3)
             
m3 n = n==3 || ((n>3) && (m3 (n-3)))                           
                                  
sumaImparesMala :: Int -> Int
sumaImparesMala n | n <= 2 = 1
              | mod n 2 == 0 = sumaImparesMala (n-1)
              | otherwise = sumaImparesMala (n-2) + n 
              
sumaImpares :: Int -> Int
sumaImpares n | n == 1 = 1
              | otherwise = sumaImpares (n-1) + 2*n - 1                 

--sumaImpares :: Int -> Int
--sumaImpares 0 = 0
--sumaImpares n = sumaImpares (n-1) + 2*n -1

medioFact :: Int -> Int
medioFact n | n == 1 = 1
            | n == 2 = 2
            | otherwise = medioFact (n-2) * n

sumaDigitos :: Int -> Int
sumaDigitos n | n < 10 = n
              | mod n 10 == 0 = sumaDigitos (div n 10) 
              | otherwise = sumaDigitos (div n 10) + mod n 10 
                 
digitosIguales :: Int -> Bool
digitosIguales n | n < 10 = True
                 | decenas == unidades = True
                 | decenas /= unidades = False
                 | otherwise = digitosIguales (div n 10)                        
                  where unidades = mod n 10
                        decenas = mod (div n 10) 10             
