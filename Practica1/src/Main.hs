cuadrado :: Int -> Int
cuadrado x = x * x

sumaLista :: [Int] -> Int
sumaLista [] = 0
sumaLista (x:xs) = x + sumaLista xs
