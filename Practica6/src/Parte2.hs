{-# OPTIONS_GHC -fno-warn-tabs #-} 
import Data.List
import Data.Char

type Mensaje = String

--Ejercicio2 : funciones minusculaAint y mayusculaAint
minusculaAint :: Char -> Int
minusculaAint x = fromEnum x - 97

mayusculaAint :: Char -> Int
mayusculaAint x = fromEnum x - 65

intAminuscula :: Int -> Char
intAminuscula x = toEnum (x + 97)

intAmayuscula :: Int -> Char
intAmayuscula x = toEnum (x + 65)

--Ejercicio 3: desplaza toma un entero y un caracter y devuelve el caracter tras desplazar a la deracha
--fromEnum a-z: 97-122
--fromEnum A-Z: 65-90

desplaza:: Int -> Char -> Char
desplaza n c
    |elem c ['a'..'z'] =intAminuscula((minusculaAint(c) + n) `mod` 26) 
    | elem c ['A'..'Z'] = intAmayuscula ((mayusculaAint c+n) `mod` 26)
    | otherwise         = c

codifica :: Int -> Mensaje -> Mensaje
codifica n [] = []
codifica n (m:msj) = desplaza n m : codifica n msj  

porcentaje :: Int -> Int -> Float
porcentaje n m = (fromIntegral n) / (fromIntegral m) * 100

--fromEnum ' ' = 32
soloLetras :: String -> String
soloLetras xs = [x| x<- xs, fromEnum x /=32]

ocurrencias letra palabra = length [1 | x <- palabra, toLower x == letra]

--Ejercicio 8: Hay 23 letras en el abedecadio, supon q la I aparece 6 veces.
--Entonces, en la posicion 8 (la correspondiente de la i en el abecedario) , ponemos
-- 6/23*100 = 26.1

ocurrenciasLista :: String -> String -> [Int]
ocurrenciasLista [] ys = []
ocurrenciasLista (y:ys) palabra = [ocurrencias y palabra] ++ ocurrenciasLista ys palabra

--En este punto:
-- ocurrenciasLista ['a'..'z'] "Educaciono" = [1,0,2,1,1,0,0,0,1,0,0,0,0,1,2,0,0,0,0,0,1,0,0,0,0,0]
trunca2dec :: Float -> Float
trunca2dec x = fromIntegral (truncate (x * 100)) / 100

frecuencias :: String -> [Float]
frecuencias xs = [trunca2dec ((fromIntegral x) / 23 * 100) | x <- ocurrenciasLista ['a'..'z'] xs]

rota:: Integer -> String -> String
rota 0 xs = xs
rota n (x:palabra) = rota (n-1) (palabra ++ [x])


