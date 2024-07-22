--Ejercicio1


satisfaceCollatz :: Integer -> Integer -> Bool
satisfaceCollatz n m | m == 0 = False  --si m llega a 0, entonces no logro llegar a 1 en m pasos, y salta False
                     | n == 1 = True   --en el caso de que n llegue a 1, salta True
                     | mod n 2 == 0 = satisfaceCollatz (div n 2) (m-1)  --en base a paridad aplico sucesion corresp. y disminuyo m por 1
                     | mod n 2 /= 0 = satisfaceCollatz (3*n + 1) (m-1)                                                                          


---Ejercicio2


auxSatisfaceCollatzEntre :: Integer -> Integer -> Integer -> Bool  --me dice si todos los numeros entre a y b satisfacen collatz en m pasos
auxSatisfaceCollatzEntre a b m | a == b = satisfaceCollatz b m --si a == b, hasta a-1 se cumplió collatz, entonces el estado de verdad recae en que b lo cumpla (en m pasos)
                               | satisfaceCollatz a m == True = auxSatisfaceCollatzEntre (a+1) b m  --si sC se cumple para dicho número, hago el test de vuelta con a+1
                               | satisfaceCollatz a m == False = False  --si sC falla para algún número entre a y b, tira False                       

satisfaceCollatzHasta n m = auxSatisfaceCollatzEntre 1 n m --uso la función auxiliar con a = 1, así mi "contador" empieza en 1
                                                           --y en cada iteracion se le suma 1, llegando a n


--Ejercicio3


auxCantidadTerminosParesContador :: Integer -> Integer -> Integer --devuelve la cantidad de terminos pares de la secuencia de collatz empezada en n, sumada a c
auxCantidadTerminosParesContador n c | n == 1 = c  --una vez que n llego a 1, devuelvo la cantidad de veces que n fue par sumadas a c
                                     | mod n 2 == 0 = auxCantidadTerminosParesContador (div n 2) (c+1) --c incrementa por 1 cuando n es par, y luego la secuencia sigue
                                     | mod n 2 /= 0 = auxCantidadTerminosParesContador (3*n + 1) c 

cantidadTerminosPares n = auxCantidadTerminosParesContador n 0 --hago el contador 0, entonces me devuelve directamente la cantidad de terminos pares


--Ejercicio4


auxLargoSecuenciaContador :: Integer -> Integer -> Integer --análogo a auxCTPC, devuelve el largo de la secuencia + un contador c
auxLargoSecuenciaContador n c | n == 1 = c --cuando llego a 1, devuelvo el largo de la secuencia + c
                              | mod n 2 == 0 = auxLargoSecuenciaContador (div n 2) (c+1) --c incrementa por 1 en cada paso de la secuencia
                              | mod n 2 /= 0 = auxLargoSecuenciaContador (3*n + 1) (c+1)

largoSecuencia n = auxLargoSecuenciaContador n 0


--Ejercicio5

auxSecuenciaMasLargaEntre :: Integer -> Integer -> Integer -> Integer --m con secuencia mas larga entre a y n
auxSecuenciaMasLargaEntre a n g | a == n = g
                                | largoSecuencia a <= largoSecuencia (a+1) = auxSecuenciaMasLargaEntre (a+1) n (a+1)
                                | otherwise = auxSecuenciaMasLargaEntre (a+1) n (a)

secuenciaMasLargaHasta n = auxSecuenciaMasLargaEntre 0 n 0