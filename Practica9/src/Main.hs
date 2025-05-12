import ListaPar (ListaP2(..), comprimida, expandida)

type ListaPCh = ListaP2 Char

tuplaACadena :: (Int, Char) -> String
tuplaACadena (a,b) = show a ++ [b]

listaAcadena :: ListaPCh -> String
listaAcadena (Datos2 []) = ""
listaAcadena (Datos2 (x:xs)) = tuplaACadena x ++ listaAcadena (Datos2 xs)

cadenaComprimida :: String -> String
cadenaComprimida xs = listaAcadena (comprimida xs)
