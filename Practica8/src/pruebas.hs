{-# OPTIONS_GHC -fno-warn-tabs #-} 
import Data.Char
import Foreign.C (CWchar)

main :: IO()
--main = putStrLn "Hola, mundo!"
--main = appendFile "D:\\2024-2025-2º\\GitHub\\PIA\\Practica8\\src\\tabla.txt" (show[(x,x*x) | x<-[1..10]])
--main = interact (map toUpper)
--main = putStrLn "Hola" >> putStrLn "mundo"

putCharLn :: Char -> IO()
putCharLn c = putChar c >> putChar '\n'
-- main = do
--     c <- getChar
--     putCharLn c

--main = print 2020

-- main = do
--     nombre <- getLine 
--     putStrLn nombre

--Ejemplo de >>
--main = putChar 'a' >> putChar 'h' >> putChar '\n'

--Ejemplo de >>= 
--main = getLine >>= print
main = getChar >>= print