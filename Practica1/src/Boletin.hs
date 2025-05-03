{-# OPTIONS_GHC -fno-warn-tabs #-} 


-- 1. Definir una funcion maximo que devuelva el mayor de sus dos argumentos. Definir
--la funcion de forma no currificada y currificada

--Version uncurry
maximo :: (Integer, Integer) -> Integer
maximo (x,y) = if x > y then x else y

--Version curry
maximoCurry :: Integer -> Integer -> Integer
maximoCurry x y = if x > y then x else y

--Version curry
maximoCurry2 :: Integer -> (Integer -> Integer)
maximoCurry2 x y = if x > y then x else y

--2. Definir una funcion que devuelva un tipo Float y calcule el area de un cırculo, dado
--su radio (usar 22/7 o la constante predefinida pi como aproximacion del numero π).

areaCirculo :: Integer -> Float
areaCirculo r = 22/7 * fromInteger(r)^2 --FromInteger transforma en el tipo necesario que sea, en este caso en un float.

--3. Sucesion de Fibonacci por encaje de patrones.

fibonacci1 :: Integer -> Integer
fibonacci1 1 = 0
fibonacci1 2 = 1
fibonacci1 n = fibonacci1(n-1) + fibonacci1 (n-2)

--3. Sucesion de Fibonacci con guardas.Applicative

fibonacci2 :: Integer -> Integer
fibonacci2 n 
    |n <= 0 = error "Error: El número debe ser mayor que 0"
    |n == 1 = 0
    |n == 2 = 1
    |otherwise = fibonacci2(n-1) + fibonacci2 (n-2)

-- 4. Aumentar y Aumentar 2
--Aumentar: devuelve el cuadrado de (x+1). Aumentar 2 = 9. 
--Aumentar2 : devuelve el siguiente del cuadrado. Aumentar2 2 = 5. 

aumentar :: Integer -> Integer
aumentar x = (x+1)^2

aumentar2 :: Integer -> Integer 
aumentar2 x = x^2 + 1

--Ahora definir sucesor y cuadrado y volver a definir ambas como composicion de las funciones cuadrado y sucesor
--sin usar varible de entrada.

sucesor :: Integer -> Integer
sucesor x  = x+1

cuadrado :: Integer -> Integer
cuadrado x = x*x

aumentarC::Integer -> Integer
aumentarC = cuadrado . sucesor -- Cojo el cuadrado del numero q entra y calculo su sucesor

aumentar2C::Integer -> Integer
aumentar2C = sucesor . cuadrado --Cojo el sucesor y le aplico el cuadrado.

--6. nAnd de dos formas diferentes

    --6.1. Con if
nAnd1:: Bool -> Bool -> Bool 
nAnd1 x y = if x == True && y == True then False else True

    --6.2. con pattern matching
nAnd2:: Bool-> Bool -> Bool 
nAnd2 True True = False
nAnd2 True False = True
nAnd2 False True = True
nAnd2 False False = True


    --6.3. con guardas
nAnd3:: Bool-> Bool -> Bool 
nAnd3 x y
    |x ==True && y==True = False
    |otherwise = True

    --6.4. case
nAnd4 :: Bool -> Bool -> Bool
nAnd4 x y = case (x, y) of
    (True, True) -> False
    otherwise -> True

-- 8. minimoTres: calcula el minimo de tres numeros

minimoTres :: Integer-> Integer-> Integer-> Integer
minimoTres x y z
    | x<y && x<z = x
    | y<x && y<z = y
    | otherwise = z

--10. numeroCentral: devuelve entre tres numeros el q tiene valor intermedio a de los otros dos.Applicative

numeroCentral :: Integer -> Integer -> Integer -> Integer
numeroCentral x y z 
    |(x > y && x < z) || (x < y && x > z) = x 
    |(y > x && y < z) || (y < x && y > z) = y
    |otherwise = z

--11. productoRango: dado dos numeros naturales n y m

factorial :: Integer -> Integer
factorial 0 = 1
factorial n = n * factorial (n-1)

productoRango :: Integer -> Integer -> Float
productoRango n m = if n > m then 0 else fromIntegral (factorial m) / fromIntegral (factorial n)

--12. Definicion rescursiva de la multiplicacion de numeros naturales. 
prod :: Int -> Int -> Int
prod n m 
    |m == 0 = 0
    |otherwise = n + prod n (m-1)