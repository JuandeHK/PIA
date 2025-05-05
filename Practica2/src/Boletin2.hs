{-# OPTIONS_GHC -fno-warn-tabs #-} 
import ImagenesSVG
import ImagenesSVG (Imagen)

--Ejercicio 1
cuatroImg :: Imagen -> Imagen
cuatroImg img = (img `junto_a` giraV (invierte_color img))   `encima` (invierte_color img `junto_a` giraV img)

--Ejercicio 2
ajedrez :: Integer -> Integer -> Imagen
ajedrez f c
    |ajedrez 0 0 = 0
    |ajedrez f 0 = (ajedrez f 0) `encima` (ajedrez (f-1) 0)
    |ajedrez f c = img + ajedrez f (c-1)  