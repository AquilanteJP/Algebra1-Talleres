doble x = 2*x 

suma x y = x+y

normaVectorial a1 a2 = sqrt(a1**2 + a2**2)

ocho = 8

signo n | n > 0 = 1
        | n == 0 = 0
        | n < 0 = (-1)
        
maximo a b | a > b = a
           | b > a = b
           | otherwise = a        

signocheto 0 = 0
signocheto n | n > 0 = 1
             | n < 0 = -1         
             
cantidadSolCuad a b c | b**2-4*a*c > 0 = 2
                      | b**2-4*a*c == 0 = 1
                      | otherwise = 0              

--cantidadSolCuad a b c | b**2-4*a*c > 0 = 2
                   --   | b**2-4*a*c == 0 = 1
                   --   | otherwise = 0                           
                   --   where d = b**2-4*a*c       otra forma para abreviar
                   
                   --   where d = b**2
                   --         e = 4*a*c
                   
absoluto :: Int -> Int
absoluto a | a >= 0 = a
           | otherwise = (-a)
           
maximoAbsoluto :: Int -> Int -> Int
maximoAbsoluto a b | (absoluto a) >= (absoluto b) = (absoluto a)
                   | otherwise = (absoluto b)   
                   
maximo3 :: Int -> Int -> Int -> Int
maximo3 a b c | (maximo a b) >= c = (maximo a b)
              | (maximo a b) <= c = c 
             
              
algunoEs0 :: Float -> Float -> Bool
algunoEs0 a b | a == 0 || b == 0 = True
              | otherwise = False
                                       
algunoEs0PM :: Float -> Float -> Bool
algunoEs0PM a 0 = True
algunoEs0PM 0 b = True
algunoEs0PM a b = False
                                          
ambosSon0 :: Float -> Float -> Bool
ambosSon0 a b = a == 0 && b == 0

ambosSon0PM :: Float -> Float -> Bool
ambosSon0PM 0 0 = True
ambosSon0PM _ _ = False

esMultiploDe :: Int -> Int -> Bool
esMultiploDe a b = (mod a b) == 0 || (mod b a) == 0

digitoUnidades :: Int -> Int
digitoUnidades n = (mod n 10)

digitoDecenas :: Int -> Int
digitoDecenas n = digitoUnidades (div n 10)
          
