{-# OPTIONS_GHC -fno-warn-tabs #-} 
import GHC.Generics ((:+:)(R1))

--EJERCICIO 1: COMO RENOMBRAMIENTO DE PAR DE ENTEROS.
type Numerador = Integer
type Denominador = Integer

type Racional = (Numerador, Denominador) --Creo el tipo de datos RACIONAL como renombramiento de un par de enteros.



simplificaRac :: Racional -> Racional
simplificaRac (a,b) = (a `div` mcdAB, b `div` mcdAB)
    where mcdAB = gcd a b
--simplificaRac (a,b) = (a `div` (gcd a b), b `div` (gcd a b))

multRac :: Racional -> Racional -> Racional
multRac (num1, den1) (num2, den2) = simplificaRac (num1 * num2 , den1 * den2)

divRac :: Racional -> Racional -> Racional
divRac (num1, den1) (num2, den2) = simplificaRac(num1 * den2 , num2* den1)

sumRac :: Racional -> Racional -> Racional
sumRac (num1, den1) (num2, den2) = ((mcmAB `div` den1) * num1  + ((mcmAB `div` den2) * num2) , mcmAB)
    where mcmAB = lcm den1 den2

resRac :: Racional -> Racional -> Racional
resRac (num1, den1) (num2, den2) =  ((mcmAB `div` den1) * num1  - ((mcmAB `div` den2) * num2) , mcmAB)
    where mcmAB = lcm den1 den2

muestraRacional :: Racional -> String
muestraRacional (n1,d1)
    |d2 == 0 = show n1
    |otherwise = show n2 ++ "/" ++ show d2
        where (n2,d2) = simplificaRac(n1,d1)

--EJERCICIO 2: COMO NUEVO TIPO DE DATOS: TIPO PRODUCTO

--data Racional2 = Rac Numerador Denominador
data Racional2 = Datos{
    num :: Numerador,
    den :: Denominador
} --deriving Show --Necesario pq sino haskell no sabe como printear.


--Datos para probar
ej = Datos{num = 6,den = 12}
ej1 = Datos{num = 1,den = 4}
ej2 = Datos{num = 4 , den = 5} 

simplificaRac2 :: Racional2 -> Racional2 --num entre mcd y den entre mcd 
simplificaRac2 r1 = Datos{
    num= num r1  `div` mcdAB,
    den = den r1 `div` mcdAB
    }
    where mcdAB = gcd (num r1) (den r1)

multRac2 :: Racional2 -> Racional2 -> Racional2 
multRac2 r1 r2 = simplificaRac2(
    Datos {
        num = num r1 * num r2,
        den = den r1  * den r2 
})

divRac2 :: Racional2 -> Racional2 -> Racional2
divRac2 r1 r2 = simplificaRac2(
    Datos {
        num = num r1 * den r2, 
        den = den r1 * num r2
})

sumRac2 :: Racional2 -> Racional2 -> Racional2
sumRac2 r1 r2 = simplificaRac2 (
    Datos {
        num = mcmR1R2 `div` den r1 * num r1 + mcmR1R2 `div` den r2 *num r2,
        den = mcmR1R2
    })
    where mcmR1R2 = lcm (den r1) (den r2)

resRac2 :: Racional2 -> Racional2 -> Racional2
resRac2 r1 r2 = simplificaRac2 (
    Datos {
        num = mcmR1R2 `div` den r1 * num r1 - mcmR1R2 `div` den r2 *num r2,
        den = mcmR1R2
    })
    where mcmR1R2 = lcm (den r1) (den r2)

-- ¿Qué significa "definir Racional como una instancia de Show"?

--Significa que le tengo que decir a haskell como convertior un valor de tipo RACIONAL2 e una cadena de texto pa imprimir.

--La forma simplficada es basicamente separadas por / y reducido.

--Para esto, tengo que crear una instancia de tupo show para mi tipo Racional2, y luego, dentro de esta instancia,
-- hacer que show use tambien simplificaRac.

--Recordar: una isntancia declara que un tipo concreto pertenece a una clase y define como funcionan sus métodos.
instance Show Racional2 where
    show r1 
        |den reducido == 1 = show (num reducido)
        |otherwise = show (num reducido) ++ "/" ++ show (den reducido) 
        where reducido = simplificaRac2 r1

instance Num Racional2 where
    (*) =  multRac2
    (+) = sumRac2
    (-) = resRac2
    negate r = Datos{
        num = -num r, 
        den = -den r
    }

    fromInteger r = Datos{
        num = r,
        den = 1
    }
    
    abs r1 = Datos {
        num = abs (num r1),
        den = abs (den r1)
    }

    signum r1
        |num r1 < 0 = -1
        |otherwise = 1 


--EJERCICIO 2:

data Nat = Cero | Succ Nat 
    deriving (Eq,Show) --pa comparar e imprimir
--Cero: 0 
--Suc 0: 1
--Suc Suc 0 : 2

instance Num Nat where
	n + Cero = n
	n + Succ m = Succ (n + m)
	n * Cero = Cero
	n * Succ m = n * m + n
	abs n = n
	signum Cero = Cero
	signum (Succ n) = Succ Cero
	n - Cero = n
	Cero - Succ m = Cero  
	Succ n - Succ m = n - m
	fromInteger x
		| x <= 0 = Cero