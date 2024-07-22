sumaDivisoresHasta :: Int -> Int -> Int
sumaDivisoresHasta n k | n == 1 || k==1 = 1 --el código estaba bien pero considera bien el caso base; k == 1 también devuelve 1, sin importar n
                       | mod n k == 0 = sumaDivisoresHasta n (k-1) + k
                       | mod n k /= 0 = sumaDivisoresHasta n (k-1)

    

sumaDivisores n = sumaDivisoresHasta n n 

menorDivDesde :: Int -> Int -> Int  --encuentra el minimo div de n, empezando a partir de l
menorDivDesde n l | mod n l /= 0 = menorDivDesde n (l+1)
                  | mod n l == 0 = l
                  
menorDiv n = menorDivDesde n 2                   
                  
esPrimo :: Int -> Bool
esPrimo n | n == 1 = False
          | menorDiv n == n = True
          | otherwise = False 
           
primoSig :: Int -> Int --encuentra el primo más cercano siguiente a un número dado
primoSig n | esPrimo (n+1) == True = n+1
           | otherwise = primoSig (n+1)           

nEsimoPrimo :: Int -> Int --la idea es aplicar primoSig n veces, empezando de n-1 hasta llegar a 1, de tal forma, consigo el n-esimo primo (2,3,..., n-esimo)
nEsimoPrimo n | n == 1 = 2
              | otherwise = primoSig (nEsimoPrimo (n-1))     

factorial :: Int -> Int
factorial n | n == 1 = 1
            | otherwise = factorial (n-1)*n

factOSig :: Int -> Int -> Int --es la función que yo quiero pero con un contador, que encuentra el siguiente factorial (o el mismo numero si es factorial) desde mi contador q
factOSig n q | n > factorial q = factOSig n (q+1)
             | otherwise = factorial q 
           
menorFactDesde n = factOSig n 1

factOAnt :: Int -> Int -> Int --hace el mismo checkeo 
factOAnt n q | n >= factorial q = factOAnt n (q+1)
             | otherwise = factorial (q-1)
             
mayorFactDesde n = factOAnt n 1       

esFact n = (menorFactDesde n == mayorFactDesde n)

fibonacci :: Int -> Int 
fibonacci n | 
                                               
