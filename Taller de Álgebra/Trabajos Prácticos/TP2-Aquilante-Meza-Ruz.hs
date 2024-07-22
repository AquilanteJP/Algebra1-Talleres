type Polinomio = [Float]
type Monomio   = (Float, Int)

--1
crearPolinomio :: [Float] -> Polinomio
crearPolinomio (0:p) = crearPolinomio p
crearPolinomio   p   = p

--2
grado :: Polinomio -> Int
grado [p] = 0
grado  p  = 1 + grado(tail p)

--3
evaluar :: Polinomio -> Float -> Float
evaluar  [ ]  x = 0
evaluar (a:p) x = a * x ^ grado(a:p) + evaluar p x

--4
productoPorMonomio :: Monomio -> Polinomio -> Polinomio
productoPorMonomio (a,n) p = producto (a:zeros n) p
             where zeros 0 = [ ]
                   zeros n =  0 : zeros(n-1)

--5
producto :: Polinomio -> Polinomio -> Polinomio
producto a [] = []
producto a (b:bs) = fila a [b] bs where fila [ ] b   x    = []
                                        fila  a  b  [ ]   = colu a b : fila (tail a)   b  []
                                        fila  a  b (x:xs) = colu a b : fila       a (x:b) xs
                                        colu (a:as)(b:bs) = a*b + colu as bs
                                        colu  a     b     = 0

--6
evaluacionMultiple :: [Float] -> Polinomio -> Polinomio -> [Float]
evaluacionMultiple   []   p q = []
evaluacionMultiple (x:xs) p q | even(grado(x:xs)) = evaluar p x : evaluacionMultiple xs p q
                              | otherwise         = evaluar q x : evaluacionMultiple xs p q
