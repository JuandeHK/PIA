module ListaPar ( ListaP2(..), expandida, comprimida,aux1 ) where
{-# OPTIONS_GHC -fno-warn-tabs #-}

import Data.List(group)

data ListaP a = Datos {
    valor :: [(Int, a)]
}

listaP1 :: ListaP Char
listaP1 = Datos [(1,'a'), (2,'b'), (3,'c')]

instance (Show a ) => Show (ListaP a) where
    show (Datos []) = []
    show (Datos [(e,v)]) = show e ++ "->" ++ show v
    show ( Datos ((e,v):xs)    ) = show e ++ "->" ++ show v ++ "\n" ++ show (Datos xs)    

-- type ListaP2 a =  ListaP a

-- listaP2 :: ListaP2 Char
-- listaP2 = Datos [(1,'a'), (2,'b')]

data ListaP2 a = Datos2 {
    valor2 :: [(Int, a)]
}deriving (Show)


-- ghci> group [1,1,7,7,7,5,5,7,7,7,7]
-- [[1,1],[7,7,7],[5,5],[7,7,7,7]]

aux1 :: [[a]] -> [(Int, a)]
aux1 [] = []
aux1 (x:xs) = (length x, head x) : aux1 xs

comprimida :: (Eq a)=> [a] -> ListaP2 a
comprimida xs = Datos2 (aux1 (group xs))

-- (2,1) = [1,1]
aux2 :: (Int,b) -> [b]
aux2 (0,y) = []
aux2 (x,y) = y : aux2 ((x-1),y) 

expandida :: ListaP2 a -> [a]
expandida (Datos2 [])= []
expandida (Datos2(x:xs)) = aux2 x ++ expandida (Datos2 xs)