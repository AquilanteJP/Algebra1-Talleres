--head te devuelve el primer elemento de una lista
--tail te devuelve una lista sin el primer elemento
--no se que hacen los dos puntos (:) => elem : lista , agrega un elemento al principio de una lista.
--mis listas solo pueden tener un tipo de variable, no puedo mezclar bool e int x ej.

longitud :: [Int] -> Int
longitud lista | lista == [] = 0
               | otherwise = 1 + longitud (tail lista)

sumatoria :: [Int] -> Int
sumatoria lista | lista == [] = 0
                | otherwise = (head lista) + sumatoria (tail lista)

pertenece :: Int -> [Int] -> Bool
pertenece n lista | lista == [] = False
                  | n == (head lista) = True
                  | n /= (head lista) = pertenece n (tail lista) 
                  
--puedo hacer listas grandes fácil, x ej: [1..100] devuelve la lista de numeros del 1 al 100 inclusive en ambos extremos, [1,3..100] devuelve los numeros impares desde 1 a 100 (empiezo con 1, y al colocar como sig elemento 3 defino un paso de 2, entonces devuelvo numeros de la pinta 1 +2n, los impares).
--no se para que corno sirve una lista de [100..1], devuelve lista vacía. pero si hago [9,6..(-10)] me devuelve múltiplos de 3 hasta el -10. => Resulta que debo especificar el paso, introduciendo el segundo elemento

--[1,0..(-100)] 

primerMultiplode45345 :: [Int] -> Int
primerMultiplode45345 lista | lista == [] = undefined
                            | mod (head lista) 45345 == 0 = head lista
                            | otherwise = primerMultiplode45345 (tail lista)

sumatoriaPM :: [Int] -> Int
sumatoriaPM [] = 0
sumatoriaPM (x:xs) = sumatoriaPM xs + x  
--se identifica la lista como (x:xs)=[x, xs1, ..., xsn] y se devuelve efectivamente la sumatoria evaluada en el tail (xs) + el head (x), como en el caso sin PM

longitudPM :: [Int] -> Int
longitudPM [] = 0
longitudPM (_:xs) = 1 + longitud xs --la misma idea pero no importa que es x, el ultimo elemento, simplemente se suma 1

pertenecePM :: Int -> [Int] -> Bool
pertenecePM _ [] = False
pertenecePM x (y:ys) | x == y = True
                     | x /= y = pertenecePM x ys  

productoria :: [Int] -> Int
productoria lista | lista == [] = 1
                  | otherwise = (head lista) * productoria (tail lista)

productoriaPM :: [Int] -> Int  --aún con PM se pueden usar guardas, PM hace referencia a como nombro mis variables??? no se, pero se pueden usar guardas
productoriaPM [] = 1  
productoriaPM (x:xs) = productoriaPM xs * x

sumarN :: Int -> [Int] -> [Int]
sumarN n lista | lista == [] = []
               | otherwise = (head lista + n):sumarN n (tail lista)

sumarNPM :: Int -> [Int] -> [Int]
sumarNPM _ [] = []
sumarNPM x (y:ys) = (y + x):sumarNPM x ys               

sumarElPrimero lista = sumarN (head lista) lista

sumarElPrimeroPM (x:xs) = sumarNPM x (x:xs)

ultimoElemLista :: [Int] -> Int
ultimoElemLista lista | longitud lista == 1 = head lista
                      | otherwise = ultimoElemLista (tail lista)

sumarElUltimo lista = sumarN (ultimoElemLista lista) lista                      

ultimoElemListaPM :: [Int] -> Int
ultimoElemListaPM (x:[]) = x
ultimoElemListaPM (x:xs) = ultimoElemListaPM xs

sumarElUltimoPM (xs) = sumarNPM (ultimoElemListaPM xs) xs
                
pares :: [Int] -> [Int]
pares lista | lista == [] = []
            | mod (head lista) 2 == 0 = (head lista):pares (tail lista)
            | otherwise = pares (tail lista)  

paresPM :: [Int] -> [Int]
paresPM [] = []
paresPM (x:xs) | mod x 2 == 0 = x:pares (xs)
               | otherwise = pares (xs)   
               
multiplosDeN :: Int -> [Int] -> [Int]               
multiplosDeN n lista | lista == [] = []
                     | mod (head lista) n == 0 = (head lista) : multiplosDeN n (tail lista)
                     | otherwise = multiplosDeN n (tail lista)

multiplosDeNPM :: Int -> [Int] -> [Int]
multiplosDeNPM n [] = []
multiplosDeNPM n (x:xs) | mod x n == 0 = x : multiplosDeNPM n xs
                        | otherwise = multiplosDeNPM n xs 

quitar :: Int -> [Int] -> [Int]
quitar n lista | lista == [] = []
               | head lista /= n = head lista : quitar n (tail lista)
               | otherwise = tail lista

quitarPM :: Int -> [Int] -> [Int]
quitarPM n [] = []
quitarPM n (x:xs) | x /= n = x : quitar n xs
                  | otherwise = xs 

hayIgualesA :: Int -> [Int] -> Bool
hayIgualesA n lista | lista == [] = False
                    | head lista == n = True
                    | head lista /= n = hayIgualesA n (tail lista)

hayRepetidos :: [Int] -> Bool
hayRepetidos lista | lista == [] = False
                   | pertenece (head lista) (tail lista) == True = True
                   | otherwise = hayRepetidos (tail lista)

quitarRepetidos :: [Int] -> [Int]
quitarRepetidos lista | lista == [] = []
                      | pertenece (head lista) (tail lista) = head lista : quitarRepetidos (quitar (head lista) (tail lista))
                      | otherwise = (head lista) : quitarRepetidos (tail lista)


                                                         
maximo :: [Int] -> Int
maximo lista | longitud lista == 1 = head lista
             | head lista > head (tail lista) = maximo ((head lista): (tail (tail lista)) )             
             | otherwise = maximo (tail lista) 

minimo :: [Int] -> Int
minimo lista | longitud lista == 1 = head lista
             | head lista < head (tail lista) = minimo ((head lista): (tail (tail lista)) )
             | otherwise = minimo (tail lista)  
               
ordenar :: [Int] -> [Int]
ordenar lista | lista == [] = []
              | head lista <= minimo lista = head lista : ordenar (tail lista)
              | otherwise = ordenar (minimo lista : (quitar (minimo lista) lista))
              
reverso :: [Int] -> [Int]
reverso lista | lista == [] = []
              | otherwise = [ultimoElemLista lista] ++ reverso (quitar (ultimoElemLista lista) lista)             
              
              
              
              
              
              
                                                                                                
