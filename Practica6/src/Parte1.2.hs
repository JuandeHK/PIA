{-# OPTIONS_GHC -fno-warn-tabs #-} 

type Persona = String
type Libro = String
type NumEjem = [(Libro, Int)]
type BD = [(Persona, Libro)] --Lista de tuplas: [("Juan", "El Quijote"), ("Ana", "1984")]

--A las funciones hay que pasarle la BD porq en haskell no hay memoria. Todo debe recibirse como argumento.
bdEjemplo :: BD
bdEjemplo = [("Ana", "1984"), ("Juan", "El Quijote"), ("Ana", "Cien anos de soledad"), ("Pedro", "El Quijote")]

numEjem :: NumEjem
numEjem = [("El Quijote", 2), ("La lista", 1), ("Luna de pluton", 2)]

numLibrosDisponibles :: NumEjem-> Libro -> Int
numLibrosDisponibles [] l1 = 0
numLibrosDisponibles ((x1,x2):xs) l1
    |x1 == l1 = x2
    |otherwise = numLibrosDisponibles xs l1

--disponibleLibro: para comprobar si hay algun ejemplar disponible
disponibleLibro :: NumEjem-> Libro -> Bool
disponibleLibro [] l1 = False
disponibleLibro ((x1,x2):xs) l1
    |x1 == l1 && x2>0 = True
    |otherwise = disponibleLibro xs l1

disponibleLibro2 :: NumEjem-> Libro -> Bool
disponibleLibro2 ne l = not(null([(x,y)| (x,y)<- ne,x==l, y>0]))

--Añadir nuevo ejemplar a la biblioteca
nuevoEjemplar :: NumEjem -> Libro -> NumEjem
--nuevoEjemplar xs l = if disponibleLibro xs l then [(x, y+1)|(x,y)<- xs, x == l] ++ xs else xs ++ [(l, 1)]
nuevoEjemplar [] l = [(l,1)]
nuevoEjemplar ((x1,x2):xs) l = if x1 == l then ((x1,x2+1) : xs ) else (x1,x2): nuevoEjemplar xs l 

extraeEjemplar:: NumEjem -> Libro -> NumEjem
extraeEjemplar [] _ = []
extraeEjemplar ((x1,x2):xs) l = 
    if disponibleLibro ((x1, x2) : xs) l then 
        if x1 == l then (x1, (x2 - 1)) : xs 
        else (x1, x2) : extraeEjemplar xs l
    else error ("No hay ejemplares de " ++ l)

devuelveEjemplar :: NumEjem -> Libro -> NumEjem
devuelveEjemplar ne l = nuevoEjemplar ne l 