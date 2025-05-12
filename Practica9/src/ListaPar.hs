{-# OPTIONS_GHC -fno-warn-tabs #-}

module ListaPar (ListaP2(..), comprimida, expandida) where

import Data.List (group)
import Data.List (sort)

data ListaP a = Datos {
    valor :: [(Int,a)]
}

lista1 :: ListaP Char
lista1 = Datos [(1,'a'),(2,'b'),(3,'c')]

instance (Show a) => Show (ListaP a) where
    show(Datos []) = ""
    show (Datos [(int,letra)]) = show int ++ "->" ++ show letra
    show (Datos((int,letra): xs)) = show int ++ "->" ++ show letra ++ "\n" ++ show (Datos xs)

main = print lista1

data ListaP2 a = Datos2 {
    valor2 :: [(Int,a)]
}deriving (Show)

-- ghci> group [1,1,7,7,7,5,5,7,7,7,7]
-- [[1,1],[7,7,7],[5,5],[7,7,7,7]]

comprimida :: (Eq a, Ord a) => [a] -> ListaP2 a
comprimida xs = Datos2[(length x, head x) | x <- group  xs]

aux :: (Int, a) -> [a]
aux (0, carac) = []
aux (rep, carac) = carac : aux (rep-1, carac)

expandida :: ListaP2 a -> [a]
expandida (Datos2 []) = []
expandida (Datos2 (x:xs)) = aux (fst x, snd x) ++ expandida (Datos2 xs)
