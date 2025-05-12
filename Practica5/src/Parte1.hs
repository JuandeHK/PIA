import GHC.Generics ((:+:)(R1))

ordenada :: (Ord a) => [a] -> Bool
ordenada [] = True
ordenada[_] = True
ordenada (x:y:xs)
    | x<= y = True  && ordenada (y:xs)
    |otherwise = False

borrar:: (Eq a) => a -> [a] -> [a]
borrar num [] = []
borrar num (x:xs)
    | num == x = xs
    | otherwise =  x: borrar num xs 

insertar2 :: (Ord a) => a -> [a] -> [a]
insertar2 num [] = [num]

insertar2 num [x]
    |num < x = num : [x]
    |otherwise = [x,num]

insertar2 num (x:y:xs)
    |num <= x = num:x:y:xs
    |num >= x && num <= y = x:num:y:xs
    |otherwise = x : insertar2 num (y:xs)

ordInsercion :: (Ord a) => [a] -> [a]
ordInsercion [] = []
ordInsercion [x] = [x]
ordInsercion (x:xs) = insertar2 x (ordInsercion xs)

ordInsercion2 :: Ord a => [a] -> [a]
ordInsercion2 = foldr insertar2 []

minimo :: Ord a => [a] -> a
minimo = head . ordInsercion
--minimo xs = head (ordInsercion xs)

mezcla :: Ord a => [a] -> [a] -> [a]
--mezcla xs (y:ys) = insertar y (ordInsercion xs) 
mezcla [] [] = []
mezcla [] ys = ys
mezcla xs [] = xs
mezcla (x:xs) ys = mezcla xs (insertar2 x (ordInsercion ys))

mitades :: [a] -> ([a],[a])
mitades ls = (take tam1 ls, drop tam1 ls)
    where 
        tam1 = length ls `div` 2 

ordMezcla :: Ord a => [a] -> [a]
ordMezcla [] = []
ordMezcla [x] = [x]
ordMezcla xs = mezcla (ordMezcla a1) (ordMezcla b1)
    where (a1,b1) = mitades xs


esPermutacion :: Eq a => [a] -> [a] -> Bool
esPermutacion [] [] = True
esPermutacion [] ys = False
esPermutacion (x:xs) ys = elem x ys && esPermutacion xs (borrar x ys)

