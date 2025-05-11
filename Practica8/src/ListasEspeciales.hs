{-# OPTIONS_GHC -fno-warn-tabs #-} 
module ListasEspeciales where

tamMax = 280
type ListaEspacial a  = [a]

esCorta :: ListaEspacial a -> Bool
esCorta xs = length xs < tamMax

esLarga :: ListaEspacial a -> Bool
esLarga xs = not(esCorta xs)

comienzaPor :: (Eq a)=> a -> ListaEspacial a -> Bool
comienzaPor a xs = head xs == a

terminarPor :: (Eq a) => a -> ListaEspacial a -> Bool
terminarPor a xs = last xs == a

todosIguales :: (Eq a) => ListaEspacial a -> Bool
todosIguales xs = all (==head xs) xs

recortaLista :: ListaEspacial a -> ListaEspacial a
--recortaLista xs = [x | (x,i) <- zip xs [1..], i <= tamMax]
recortaLista xs = take tamMax xs