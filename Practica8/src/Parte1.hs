{-# OPTIONS_GHC -fno-warn-tabs #-} 
import Data.List (sort)

ordenarDatos :: String -> [Int]
ordenarDatos xs = sort (map read (words xs))

numMenoresLista :: Int -> [Int] -> Int
numMenoresLista num lista = length [1 | x<- lista, x<= num]

calcpercentil :: Int -> [Int] -> Float
calcpercentil num lista = fromIntegral (numMenoresLista num lista)  / fromIntegral (length lista)

quitarCorchetesComas :: [Int] -> String
quitarCorchetesComas [] =  []
quitarCorchetesComas [x] = show x
quitarCorchetesComas (x:xs) = show x ++ "" ++ quitarCorchetesComas xs

main = do
    contenido <- readFile "D:\\2024-2025-2º\\GitHub\\PIA\\Practica8\\src\\datos.txt"
    let contenidoOrdInt = ordenarDatos contenido
    --print contenidoEnStrings
    putStrLn "Dame un numero para calcular el percentil"
    --numero <- readLn :: IO Int
    entrada <- getLine
    let numero = read entrada
    let percentil = calcpercentil numero contenidoOrdInt
    --print contenidoOrdInt
    let contenidoSinCorchetesComas = quitarCorchetesComas contenidoOrdInt
    print contenidoSinCorchetesComas
