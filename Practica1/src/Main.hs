cuadrado :: Integer -> Integer
cuadrado x = x * x

minimo:: (Integer, Integer) -> Integer
minimo (x,y) = if x <= y then x else y

--Evaluacion perezosa
tres :: Integer -> Integer
tres x = 3

infinito :: Integer
infinito = infinito + 1 

-- Funciones de orden superior
dosVeces :: (Integer -> Integer, Integer) -> Integer
dosVeces (f, x) = f (f x)

--cuadrado x = x *
--dosVeces (cuadrado, 2)  -- Resultado: 256
-- porque cuadrado(cuadrado(2)) = cuadrado(4) = 1

factorial :: Integer -> Integer
factorial 0 = 1 
factorial n = n * factorial(n-1)
