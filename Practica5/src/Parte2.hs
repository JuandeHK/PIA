module Parte2 where


-- cAplica f xs = concat (map f xs)
cAplica :: (a->[b]) -> [a] -> [b]
cAplica f xs =  concat(map f xs)

-- Rota una lista una sola vez: mueve el primer elemento al final
rotaUno :: [a] -> [a]
rotaUno [] = []
rotaUno (x:xs) = xs ++ [x]

rotaLista :: [a] -> [[a]]
rotaLista xs = rotaAux xs (length xs)
    where
    rotaAux _ 0 = []
    rotaAux ys n = ys : rotaAux (rotaUno ys) (n - 1) --Si ys es una lista y rotaaux devuelve lista de listas...

--Ejercicio 3
contiene :: Eq a => [a] -> [a] -> Bool
contiene xs [] = True
contiene xs (y:ys) = elem y xs

quitacabeza :: Eq a => [a] -> [[a]] -> [[a]]
quitacabeza xs [] = []
quitacabeza xs (ys:yss) 
    | contiene xs ys == False =  ys : quitacabeza xs yss
    | otherwise = quitacabeza xs yss

--Ehercicio 4:
--Reciba un título de libro como un String
--Devuelva todas las rotaciones posibles de sus palabras
--Pero solo aquellas rotaciones que no empiezan por una palabra "trivial"


ordMezcla :: Ord a => [a] -> [a]
ordMezcla [] = []
ordMezcla [x] = [x]
ordMezcla xs = mezcla (ordMezcla a1) (ordMezcla b1)
    where (a1,b1) = mitades xs

mezcla :: Ord a => [a] -> [a] -> [a]
--mezcla xs (y:ys) = insertar y (ordInsercion xs) 
mezcla [] [] = []
mezcla [] ys = ys
mezcla xs [] = xs
mezcla (x:xs) ys = mezcla (ordInsercion xs) (insertar2 x (ordInsercion ys))


ordInsercion :: (Ord a) => [a] -> [a]
ordInsercion [] = []
ordInsercion [x] = [x]
ordInsercion (x:xs) = insertar2 x (ordInsercion xs)

--Con recursividad
insertar2 :: (Ord a) => a -> [a] -> [a]
insertar2 num [] = [num]

insertar2 num [x]
    |num < x = num : [x]
    |otherwise = [x,num]

insertar2 num (x:y:xs)
    |num <= x = num:x:y:xs
    |num >= x && num <= y = x:num:y:xs
    |otherwise = x : insertar2 num (y:xs)

mitades :: [a] -> ([a],[a])
mitades ls = (take tam1 ls, drop tam1 ls)
    where 
        tam1 = length ls `div` 2 

titulo = "introduccion a la programacion funcional con haskell"
palTriviales = ["la","las","como", "de", "a", "con", "su","el", "un","en"]
--rotaTitulo "introduccion a la programacion funcional con haskell"

rotaTitulo:: String -> [String]
--Lo primero que voy a hacer va a ser transformar la cadena de caracteres a cadena de strings con word:
    --words titulo
--Luego voy a generar todas las posibles ordenaciones de estos strings:
    --rotaLista (words titulo)
--Luego voy a filtrar los que no tienen triviales
    --quitacabeza palTriviales (rotaLista (words titulo))
--Lo transformo de lista de strings a lista de chars
rotaTitulo xs = map unwords (quitacabeza palTriviales (rotaLista (words titulo)))



--rotaTitulo :: String -> [String]
--rotaTitulo s = map unwords (quitacabeza palTriviales (rotaLista (words s)))

-- rotaTitulo s =
--     let palabras = words s
--         rotaciones = rotaLista palabras
--         rotacionesValidas = quitacabeza palTriviales rotaciones
--     in map unwords rotacionesValida



--Ejercicio 5:
--Lista ordenada de rotaciones de titulos. 

--kwic ["las palomas emprenden el vuelo","opiniones de un payaso"]

kwic :: [String] -> [String]
--Siendo x1= "las palomas emprenden el vuelo" y en la siguiente ="opiniones de un payaso"
kwic [] = []
kwic (x:xs) = ordMezcla (rotaTitulo x ++ kwic xs )

generadorListas :: Int -> [a] -> [[a]]
generadorListas 1 xs = [xs]
generadorListas n xs = xs : generadorListas (n-1) xs


--Rota una lista una sola vez: mueve el primer elemento al final
rotaLista2 :: [Int] -> [[Int]]
rotaLista2 [] = []
--rotaLista2 xs = xs : rotaUno(xs)
rotaLista2 xs = generadorListas2 (length xs)  xs 
    where 
        generadorListas2 0 _ = []
        generadorListas2 n xs = xs: generadorListas2 (n-1) (rotaUno xs)

