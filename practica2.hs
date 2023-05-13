-- Cruz Pineda Fernando

mcd :: Int -> Int -> Int
mcd a b = if (mod a b)==0 then b else mcd b (mod a b)


mcm :: Int -> Int -> Int
mcm a b = div (a*b) (mcd a b)

-- maximo :: Num a => [a] -> a
maximo [x] = x
maximo (x:z:s) = if x > z then maximo (x:s) else maximo (z:s)

divisores :: Int -> [Int]
divisores 0 = []
divisores x = x:divisores x

sumaGauss :: Int -> Int
sumaGauss 0 = 0
sumaGauss x = x + sumaGauss(x - 1)

tribonnacci :: Int -> Int
tribonnacci (-1) = 0
tribonnacci 0 = 1
tribonnacci 1 = 1
tribonnacci x = (tribonnacci (x - 1)) + (tribonnacci (x - 2)) + (tribonnacci (x - 3))

mezcla :: [Int] -> [Int] -> [Int]
mezcla (x:xs) [] = (x:xs)
mezcla [] (x:xs) = (x:xs)
mezcla (x:xs) (y:ys) = if x < y then x:(mezcla xs (y:ys)) else y:(mezcla (x:xs) ys)

reversa :: [a] -> [a]
reversa [] = []
reversa (x:xs) = reversa xs ++ [x]

encuentra x [] = False
encuentra x (y:ys) = if x==y then True else encuentra x ys

elimina x []= []
elimina x (y:ys) = if x==y then elimina x ys else y:(elimina x ys)

-- diferenciaSimetrica :: [a] -> [a] -> [a]
diferenciaSimetrica [] [] = []
diferenciaSimetrica [] (x:xs) = (x:xs)
diferenciaSimetrica (x:xs) [] =(x:xs)
diferenciaSimetrica (x:xs) (y:ys) = if (encuentra x (y:ys)) then diferenciaSimetrica xs (elimina x (y:ys)) else x:(diferenciaSimetrica xs (y:ys))

-- diferencia :: [a] -> [a] -> [a]
diferencia [] []=[]
diferencia [] xs = []
diferencia (x:xs) ys = if (encuentra x ys) then diferencia xs ys else x:(diferencia xs ys)

-- diferenciaSimetrica1 :: [a] -> [a] -> [a]
diferenciaSimetrica1 [] [] = []
diferenciaSimetrica1 xs ys = diferencia xs ys ++diferencia ys xs

conjuntoPotencia :: [a] -> [[a]]
conjuntoPotencia []= [[]]
conjuntoPotencia (x:xs)= let conjuntoPotencia2 = conjuntoPotencia xs in conjuntoPotencia2++[(x:z) |z <- conjuntoPotencia2]

f:: (a -> b) -> [a] -> [b]
f (g) [] = []
f (g) (x:xs) = (g x):(f (g) (xs))

g :: (a -> b -> c) -> [a] -> [b] -> [c]
g (m) [] []= []
g (m) (x:xs) []= []
g (m) [] (x:xs) = []
g (m) (x:xs) (y:ys) = (m x y):(g (m) xs ys)

-- Regresa la lista sin el último elemento
ultimo :: [a] -> [a]
ultimo [] = []
ultimo [a] = []
ultimo (x:xs) = [x] ++ ultimo xs

-- Regresa el último elemento de una lista
ultimo2 :: [a] -> a
ultimo2 [a] = a
ultimo2 (x:xs) = ultimo2 xs

-- palindromo :: [a] -> Bool
palindromo [] = True
palindromo [a] = True
palindromo (x:xs) = x == (ultimo2 xs) && palindromo (ultimo xs)