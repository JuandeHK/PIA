{-# OPTIONS_GHC -fno-warn-tabs #-} 
import GHC.Windows (BOOL)

--Sinonimos de tipo (RENOMBRADO): para dar nombre a un tipo ya existente -> mayor legibilidad.
type Nombre = String
type Edad = Integer
type Persona = (Nombre, Edad)
tocayos :: Persona->Persona->Bool
tocayos (nombre1, _) (nombre2, _ ) = nombre1 == nombre2

--1. DEFINICIONES DE TIPO: definir nuevos tipos de dtaos mediante la sentencia DATA.


-- 1.1. TIPO PRODUCTO

-- -------------------SIN DAR NOMBRES---------------------------------------------------------------
data Persona2 = Pers Nombre Edad -- Combinamos varios valores en una sola estructura llamada Persona2

--CREAR PERSONA
juan::Persona2 --Juan es una ca
juan = Pers "Juan Lopez" 25

--ACCESO A LOS DATOS: 
nombreJuan :: Persona2 -> Nombre
nombreJuan (Pers n _) = n

--FUNCIONES
esJoven :: Persona2 -> Bool
esJoven (Pers _ edad) = edad < 25
--a es joven se le pasa como parametro una persona2, se ignora el parametro edad y se comprueba si su edad es <25.

-- -------------------DANDO NOMBRES---------------------------------------------------------------
data Persona3 = Datos{
    nombre :: Nombre,
    edad :: Edad
}

--CREAR PERSONA
pepe:: Persona3
pepe = Datos{
    edad = 25,
    nombre="Pepe Martínez"
}

--ACCESO A LOS DATOS: 
main :: IO ()
main = do
    print (edad pepe) --ACCESO DIRECTO!!!!

--FUNCIONES: AQUI SE PUEDE VER EL ACCESO DIRECTO
tocayos3 :: Persona3 -> Persona3 -> Bool
tocayos3 p1 p2 = nombre p1 == nombre p2

-- 1.2. TIPO ENUMERADO
data Temperatura = Frio | Caliente
data Estacion = Primavera | Verano | Otoño | Invierno

tiempo :: Estacion -> Temperatura
tiempo Primavera = Caliente
tiempo Verano = Caliente
tiempo Otoño = Frio
tiempo _ = Frio -- LO QUE NO SEA NI PRIMAVERA NI VERANO NI OTOÑO

--TIPO ENUMERADO CON ALTERNATIVAS

data Forma =
    Circulo Float |Rectangulo Float Float  -- EL CUADRADO ES EL Q TIENE EL PRODUCTO, AGRUPA 2 VALORES FLOAT.

--Creo un enumerado llamado forma. Puede ser:
    --1. Un criculo con un radio (float). 
        --Circulo 3.0  -- un círculo de radio 3

    --2. Un rectangulo con ancho y alto (float float)
        --Rectangulo 4.0 5.0  -- ancho = 4, alto = 5
