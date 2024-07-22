--para representar conjuntos, podemos usar type para distinguir explicitamente a un nuevo tipo (que voy a llamar Set) de una lista. Haskell no va a verificar que sea diferente de una lista, sin elementos repetidos o si el conjunto [1,2,3] es equivalente a [3,2,1]
--si una función recibe un Set, VOY A SUPONER que no tiene elementos repetidos.
--si una función devuelve un Set, DEBO ASEGURAR que no tiene elementos repetidos.
--el orden de los elementos es irrelevante.

type Set a = [a]

vacio :: Set Int
vacio = []

agregar :: (Eq a) => a -> Set a -> Set a
agregar n set | elem n set == True = set --elem es una funcion built in de Haskell q me dice si n pertenece al set
              | otherwise = n:set
              
incluido :: Set Int -> Set Int -> Bool
incluido setA setB | setA == vacio = True
                   | elem (head setA) setB == True = incluido (tail setA) setB
                   | otherwise = False

cardinal :: Set Int -> Int
cardinal set | set == vacio = 0
             | otherwise = 1 + cardinal (tail set)
             
iguales :: Set Int -> Set Int -> Bool
iguales setA setB = incluido setA setB && cardinal setA == cardinal setB 

--funciones de clase 6

longitud :: [Int] -> Int
longitud lista | lista == [] = 0
               | otherwise = 1 + longitud (tail lista)

quitar :: Int -> [Int] -> [Int]
quitar n lista | lista == [] = []
               | head lista /= n = head lista : quitar n (tail lista)
               | otherwise = tail lista

minimo :: [Int] -> Int
minimo lista | longitud lista == 1 = head lista
             | head lista < head (tail lista) = minimo ((head lista): (tail (tail lista)) )
             | otherwise = minimo (tail lista)  

ordenar :: [Int] -> [Int]
ordenar lista | lista == [] = []
              | head lista <= minimo lista = head lista : ordenar (tail lista)
              | otherwise = ordenar (minimo lista : (quitar (minimo lista) lista))
              
--              

agregarElemATodoConj :: Int -> Set (Set Int) -> Set (Set Int)
agregarElemATodoConj n set | set == [] = [] --el [] default se puede intepretar como el "vacío mas grande", o sea, si se tiene [[]], es el de afuera; el chiste es que el [] default es equivalente a un conj con un subconj vacio, y puede irse aún mas [[[]]]...
                           | otherwise = agregar (ordenar (agregar n (head set)))  (agregarElemATodoConj n (tail set))
                          
partes :: Int -> Set (Set Int)
partes n | n == 1 = [[],[1]]
         | otherwise = partes (n-1) ++ (agregarElemATodoConj n (partes (n-1)))

quitarUltimo :: Set Int -> Set Int
quitarUltimo set | tail set == [] = []  
                 | otherwise = agregar (head set) (quitarUltimo (tail set))

generarPares :: Int -> Set Int -> Set (Int, Int)
generarPares n set | set == [] = [] 
                   | otherwise = agregar (n, head set) (generarPares n (tail set))

generarSetNatN :: Int -> Set Int
generarSetNatN n | n == 0 = []
                 | otherwise = ordenar(agregar n (generarSetNatN (n-1)))

prodCart :: Set Int -> Set Int -> Set (Int, Int)
prodCart setA setB | setB == [] = []
                   | otherwise = agregar (generarPares m (generarSetNatN n)) (prodCart setA (quitarUltimo setB))                            

                                    

                              
