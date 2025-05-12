{-# OPTIONS_GHC -fno-warn-tabs #-}
import ListaPar ( ListaP2(..), expandida, comprimida,aux1 ) 
import Data.Char (digitToInt)
import Data.Char (isNumber)
import Data.Char (isLetter)
import Data.List(nub,group,sort)



type ListaPCh = ListaP2 Char

-- listaAcadena (Datos2 [(12,'B'),(1,'N'),(12,'B'),(3,'N'),(19,'B')])

listaAcadena :: ListaPCh -> String
listaAcadena (Datos2 []) = []
listaAcadena (Datos2((x,y):xs)) = show x ++ [y] ++ listaAcadena (Datos2 xs )

cadenaComprimida :: String -> String
cadenaComprimida xs = listaAcadena (comprimida xs)

cortar :: (String, String) -> [String]
cortar (parte, []) = []  
cortar (parte, l:resto) = (parte ++ [l]) : cortarPorLetra resto

cortarPorLetra :: String -> [String]
cortarPorLetra [] = []
cortarPorLetra xs = cortar (break isLetter xs)

cadenaAlista :: String -> ListaPCh
cadenaAlista xs = Datos2 [(read (init x), last x) | x<-cortarPorLetra xs]

cadenaExpandida :: String -> String
cadenaExpandida = expandida . cadenaAlista

-- main :: IO ()
-- main = do
--     putStrLn "Pulsa una 'c' si quieres codificar una cadena o 'd' si lo que deseas es decodificarla"
--     op <- getLine
--     if op == "c"
--         then do
--         putStr "Cadena: "
--         c <- readLn
--         putStrLn (cadenaComprimida c)
--         else
--         if op == "d"
--             then do
--             putStr "Cadena: "
--             c <- readLn
--             putStrLn (cadenaExpandida c)
--             else error "Opcion incorrecta\n"


main :: IO()
main = do
    putStrLn "'c' para comprimir o 'd' para expandir:"
    letra <- getLine
    if letra == "c" then do
        putStrLn "dame la cadena"
        cadena <- getLine
        putStrLn (cadenaComprimida cadena)
    else 
        if letra == "d" then do
        putStrLn "dame la compresion"
        cadena <- getLine
        putStrLn(cadenaExpandida cadena)
        else error "mierdon"