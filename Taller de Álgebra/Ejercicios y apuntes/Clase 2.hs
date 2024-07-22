mismoTipo :: t -> t -> Bool
mismoTipo x y = True



--Formato de funciones es: nombreFunc Var1 Var2 = funcPPD

primero x y = x --devuelve la 1ra var 

segundo x y = y --devuelve la 2da var

constante5 x y z = 5



--triple :: Num t => t -> t
triple x = 3 * x

--maximo :: Ord p => p -> p -> p
maximo x y | x >= y = x
           | otherwise = y

--distintos :: Eq a => a -> a -> Bool
distintos x y = x /= y



--t debe ser Ord tmb porque se deben hacer comparaciones (mayor, menor, igual)
cantidadDeSoluciones :: (Num t, Ord t) => t -> t -> t -> Int
cantidadDeSoluciones a b c | d > 0 = 2
                           | d == 0 = 1
                           | otherwise = 0
                           where d = b^2 - 4*a*c 
                           
pepe :: (Floating t, Eq t, Num u, Eq u) => t -> t -> u -> Bool
pepe x y z = sqrt (x + y) == x && 3*z == 0 --Verifica que la raiz de x + y sea igual a x y ademas que z=0


--f1 :: (Ord a, Floating a) => a -> a -> a -> Bool
--Debe ser Ord para poder comparar, y Floating para que garantizar que funque la exponenciación?
f1 x y z = x**y + z <= x+y**z

--f2 :: Floating a => a -> a -> a
--Debe ser Floating para asegurar que sqrt devuelva Float
f2 x y = ( sqrt x ) / ( sqrt y )

--f3 :: (Integral a, Floating a) => a -> a -> a
--No funca porque una variable no puede pertenecer a la clase Integral (div, division entera) y Floating (sqrt) a la vez
--O quiza es xq div es division entera y sqrt devuelve float siempre
f3 x y = div ( sqrt x ) ( sqrt y )


--f4 :: (Eq p, Floating p) => p -> p -> p -> p
--Debe ser Eq porque hago comparaciones de igualdad y Floating porque hay exponenciación?
f4 x y z | x == y = z
         | x ** y == y = x
         | otherwise = y
         
--f5 :: (Eq a, Floating a) => a -> a -> p -> p
--La función siempre devuelve z, entonces las restricciones solo se ejercen sobre x e y; z puede ser Bool o FLoat o lo que sea
f5 x y z | x == y = z
         | x ** y == y = z
         | otherwise = z



--tuplas: (1,2) :: (Int, Int)
--(1.1, 3.2, 5.0) :: (Float, Float, Float)
--(True, (1,2)) :: (Bool, (Int, Int))
--(True, 1, 2) :: (Bool, Int, Int)

suma :: (Float, Float) -> (Float, Float) -> (Float, Float)
--suma v w = ((fst v) + (fst w), (snd v) + (snd w))
suma (vx,vy) (wx,wy) = (vx + wx, vy + wy)

tercero :: (ta,tb,tc) -> tc
tercero (a,b,c) = c

esOrigen :: (Float, Float) -> Bool
esOrigen (0,0) = True
esOrigen (_,_) = False

angulo0 :: (Float, Float) -> Bool
angulo0 (_,0) = True
angulo0 (_,_) = False

{-
angulo45 :: (Float, Float) -> Bool
angulo45 (x,x) = True
angulo45 (_,_) = False
esto no funca xq no puedo usar la misma variable 2 veces en el pattern matching
-}

angulo45 :: (Float, Float) -> Bool
angulo45 (x,y) = x == y


--que pertenezcan a la misma clase de equivalencia en R significa que pertenezcan al mismo intervalo

estanRelacionados :: Float -> Float -> Bool
--estanRelacionados :: (Ord a1, Ord a2, Num a1, Num a2) => a1 -> a2 -> Bool
estanRelacionados x y | x <= 3 && y <= 3 = True --x <= 3 = y <= 3, se puede desarrollar de esta forma tmb (no es exactamente igual)
                      | x >= 7 && y >= 7 = True --x >
                      | (x > 3 && x < 7) && (y > 3 && y < 7) = True
                      | otherwise = False
                      
prodInt :: (Float, Float) -> (Float, Float) -> Float
--prodInt :: Num a => (a, a) -> (a, a) -> a
prodInt (vx,vy) (wx, wy) = vx * wx + vy * wy                      


todoMenor :: (Float, Float) -> (Float, Float) -> Bool
--todoMenor :: (Ord a1, Ord a2) => (a1, a2) -> (a1, a2) -> Bool
todoMenor (vx,vy) (wx, wy) = vx < wx && vy < wy

distanciaPuntos :: (Float, Float) -> (Float, Float) -> Float
--distanciaPuntos :: Floating a => (a, a) -> (a, a) -> a
distanciaPuntos (vx,vy) (wx, wy) = sqrt ((vx-wx)^2 + (vy-wy)^2)

sumaTerna :: (Float, Float, Float) -> Float
--sumaTerna :: Num a => (a, a, a) -> a
sumaTerna (x,y,z) = x+y+z

posicPrimerPar :: (Int, Int, Int) -> Int
--posicPrimerPar :: (Integral a1, Integral a2, Integral a3, Num p) =>(a1, a2, a3) -> p
posicPrimerPar (x,y,z) | mod x 2 == 0 = 1
                       | mod y 2 == 0 = 2
                       | mod z 2 == 0 = 3
                       | otherwise = 4

crearPar :: a -> b -> (a,b)
--crearPar :: a -> b -> (a, b)
crearPar a b = (a,b)

invertir :: (a,b) -> (b,a)
--invertir :: (b, a) -> (a, b)
invertir (a,b) = (b,a)                      


               
