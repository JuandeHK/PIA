{-# OPTIONS_GHC -fno-warn-tabs #-} 

import ListasEspeciales
import Data.Char          -- Para 'toLower'
import Data.List           -- Para (\\)
import Distribution.Compat.Prelude (Show(show))
import ListasEspeciales (tamMax)
import Data.Char (toLower)
import Data.String (String)
import Data.List (sortBy)
import Data.Function (on)

type Dia = Integer
type Mes = Integer
type Anyo = Integer

type Fecha = (Dia,Mes,Anyo)

type Identidad = ListaEspacial Char
type Mensaje  = ListaEspacial Char --Un mensaje es como un [Char], es decir, un String
type Hashtag = ListaEspacial Char

esFecha :: Fecha -> Bool
esFecha (x,y,z) = x>=1 && x<=31 && y>=1 && y<=12 && z>=1 && z<=9999  

esIdentidad:: Identidad -> Bool
esIdentidad x = head x == '@'

esHashtag :: Hashtag -> Bool
esHashtag x = head x == '#'


data Tuit = Datos {
    fecha :: Fecha,
    identidad :: Identidad,
    mensaje :: Mensaje
}

type Tuits = [Tuit]


instance Show Tuit where --iden, como es un string no hace falta hacerle un show
    show (Datos(d,m,a) identidad mensaje) 
        | esCorta mensaje = identidad ++ " (" ++ show d ++  "," ++ show m ++  "," ++ show a ++ "): " ++ mensaje 
        | esLarga mensaje = identidad ++ " (" ++ show d ++  "," ++ show m ++  "," ++ show a ++ "): " ++ recortaLista mensaje ++ "..."
        | otherwise = error "Error en formato"

t1 :: Tuit
t1 = Datos (18, 1, 1892) "@Juande" "#Mi polla"

t2 :: Tuit
t2 = Datos (18, 1, 1892) "@Kasita" "#Mi huevos"

t3 :: Tuit
t3 = Datos (18, 1, 1892) "@Kasita" "#hey como #estas"

t4 :: Tuit
t4 = Datos (18, 1, 1892) "@Antonio" "#tu puta amdre"

main = print t1

identidades :: Tuits -> [Identidad]
-- identidades [] = []
-- identidades (x:xs) = identidad x : identidades xs
identidades xs = nub(sort [identidad x | x <- xs])

mensajes :: Tuits -> [Mensaje]
mensajes xs = [mensaje x | x <- xs]

-- ghci> mensajes [t1,t2]   
-- ["#Mi polla","#Mis huevos"]

buscaHashtagTuit :: Tuit -> [Hashtag]
buscaHashtagTuit tuit = borraRepetidos [palabra | palabra <- words (mensaje tuit) , esHashtag palabra]

borraRepetidos :: [Hashtag] -> [Hashtag]
borraRepetidos [] = []
borraRepetidos (h:hs)
    | elem (map toLower h) hs = borraRepetidos hs
    | otherwise = (map toLower h) :borraRepetidos hs 

buscaHashtag :: Mensaje -> [Hashtag] --Un mensaje es como un [Char], es decir, un String. Ej: "hola que tal" 
buscaHashtag msj = borraRepetidos[palabra | palabra <- words msj , esHashtag palabra ]

todosLosHashtags :: [Mensaje] -> [Hashtag] --Una lista de mensajes es [String] o [[Char]]. Ej ["hola", "que", "tal"]
todosLosHashtags [] = []
todosLosHashtags (msj : msjs) =  borraRepetidos (buscaHashtag msj ++ todosLosHashtags msjs)

tuitsConHashtag :: Tuits -> Hashtag -> Tuits
tuitsConHashtag [] _ = []
tuitsConHashtag (tuit:tuits) hastag
    | elem hastag (buscaHashtagTuit tuit) = tuit: tuitsConHashtag tuits hastag
    | otherwise = tuitsConHashtag tuits hastag

personasHashtag :: Tuits -> Hashtag -> [Identidad]
personasHashtag [] _ = []
personasHashtag (tuit:tuits) hastag
    | elem hastag (buscaHashtagTuit tuit) = identidad tuit: personasHashtag tuits hastag
    | otherwise = personasHashtag tuits hastag


tuitsConMismoHashtag :: Tuits -> [(Hashtag,Tuits)]
tuitsConMismoHashtag tuits = [(h, tuitsConHashtag tuits h) | h <- todosLosHashtags (mensajes tuits) ]

numtuitsConMismoHashtag :: Tuits -> [(Hashtag,Int)]
numtuitsConMismoHashtag tuits = [(h,  length(tuitsConHashtag tuits h)) | h <- todosLosHashtags (mensajes tuits) ]

ordenarPorSegundoDesc :: Ord b => [(a, b)] -> [(a, b)]
ordenarPorSegundoDesc = sortBy (flip compare `on` snd)

trendingTopic :: Tuits -> Hashtag
trendingTopic tuits = fst (head ( ordenarPorSegundoDesc ( numtuitsConMismoHashtag tuits)))

tuitsDesde :: Fecha -> Tuits -> Tuits
tuitsDesde (d,m,a) [] = []
tuitsDesde (d,m,a) (tuit:tuits)
    |fecha tuit >= (d,m,a) = tuit:tuitsDesde (d,m,a) tuits
    |otherwise = tuitsDesde (d,m,a) tuits

---
listaTuitsPersona :: Identidad -> Tuits -> [Int]
listaTuitsPersona id [] = []
listaTuitsPersona id (tuit:tuits)
    | id == identidad tuit  = [1] ++ listaTuitsPersona id tuits
    | otherwise = listaTuitsPersona id tuits

numTuitsPersona :: [Int] -> Int
numTuitsPersona = length

masActivo :: Tuits -> Identidad
masActivo tuits = fst (head (ordenarPorSegundoDesc [(id, numTuitsPersona (listaTuitsPersona id tuits)) | id <- identidades tuits]))