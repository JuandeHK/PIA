{-# OPTIONS_GHC -fno-warn-tabs #-} 

type Persona = String
type Libro = String
type BD = [(Persona, Libro)] --Lista de tuplas: [("Juan", "El Quijote"), ("Ana", "1984")]

--A las funciones hay que pasarle la BD porq en haskell no hay memoria. Todo debe recibirse como argumento.
bdEjemplo :: BD
bdEjemplo = [("Ana", "1984"), ("Juan", "El Quijote"), ("Ana", "Cien anos de soledad"), ("Pedro", "El Quijote")]

--libros: Dada una persona, obteenr los libros que tiene en prestamo.
libros :: BD -> Persona  -> [Libro]
libros bd p1 = [y|(x,y) <- bd, x == p1]

--lectores: dado un libro, obtener los libros que tiene en prestamo
lectores :: BD -> Libro -> [Persona]
lectores bd lib = [x| (x,y)<- bd, lib == y ]

--Prestado: dado un libro, obtener info sobre si esta prestado o no.
prestado :: BD -> Libro -> Bool
--prestado bd lib = if length(lectores bd lib) >0 then True else False
prestado bd lib = not(null(lectores bd lib))

--numPrestados: dada una persona, encontrar el num de libros q tiene
numPrestados :: BD-> Persona -> Int
numPrestados bd p1 = length [x | (x,y) <- bd, x==p1]
--length (libros bd p1)

--Realizar un nuevo prestamo
realizarPrestamo :: BD -> Persona -> Libro -> BD
realizarPrestamo bd p1 l1 = (p1,l1) : bd 
--realizarPrestamo bd p l = bd ++ [(p,l)]

--Devolver un prestamo
devolverPrestamo :: BD -> Persona -> Libro -> BD
devolverPrestamo bd p1 l1 = [(x,y) | (x,y) <- bd, (x,y) /= (p1,l1) ]

-- devolverPrestamo (x:xs) p1 l1 
--     |x == (p1,l1) = xs
--     |otherwise = x: devolverPrestamo xs p1 l1



