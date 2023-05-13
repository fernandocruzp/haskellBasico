-- Cruz Pineda Fernando
-- Nivel 5

casteo '1' = 1
casteo '2' = 2
casteo '3' = 3
casteo '4' = 4
casteo '5' = 5
casteo '6' = 6
casteo '7' = 7
casteo '8' = 8
casteo '9' = 9
casteo '0' = 0

data Arbol = NIL | Arbol(Arbol,Int,Arbol) deriving (Show) 
-- data Negacion = Neg deriving (Show)
data Conectivos= Y | O | Ent | Neg deriving (Show)
data WFLF = Nada | P | Q | R | Tru | Flse | WFLF(WFLF,Conectivos,WFLF) deriving (Show)
data Parentesis = PaIz | ParDer deriving (Show)
data Simb = Mas | Menos | Por | Div deriving (Show)
data EF = X | Ye  | EF(Parentesis,EF,Simb,EF,Parentesis) deriving (Show)

contarvar ::  EF -> EF -> Int
contarvar X X = 1
contarvar X Ye = 0
contarvar Ye Ye = 1
contarvar Ye X = 0
contarvar (EF(z,x,t,y,b)) a = (contarvar x a) + (contarvar y a)

-- fibo :: Int -> [Int]
-- fib 0 = 1:[]
-- fib 1 = 1:[]
-- fib x = x:(fib x-1)



swap P = (WFLF(Nada,Neg,P))
swap Q = (WFLF(Nada,Neg,Q))
swap R = (WFLF(Nada,Neg,R))
swap Tru = Tru
swap Flse = Flse
swap (WFLF(Nada,Neg,x)) = swap x
-- swap (WFLF(x,y,z)) = (WFLF(Nada,Neg,swap (WFLF(x,y,z))))
swap (WFLF(x,Y,y)) = (WFLF(swap x, O , swap y))
swap (WFLF(x,O,y)) = (WFLF(swap x, Y , swap y))

contFor P = 1
contFor Q = 1
contFor R = 1
contFor Tru = 1
contFor Flse = 1
contFor (WFLF(Nada,Neg,x)) = 1 + contFor x
contFor (WFLF(x,t,y)) = 1 + contFor x + contFor y

-- Concatena una lista con otra
concat1 [] (x:xs) = (x:xs)
concat1 (x:xs) [] = (x:xs)
concat1 (x:xs) (n:ns) = x: (concat1 xs (n:ns))

-- Otras ................................................................

espejo NIL = NIL
espejo (Arbol(x,t,y))= (Arbol(espejo y, t, espejo x))


-- Suma dos enteros
s :: Int -> Int -> Int
s n 0 = n
s n m = 1 + s n (m-1)

--Multiplica dos naturales
m :: Int -> Int -> Int
m n 0 = 0
m n p = n + (m n (p-1))

-- determina si un número es par
par :: Float -> Bool
par 0 = True
par n = impar(n-1)

impar :: Float -> Bool
impar 0 = False
impar n = par (n-1)

-- Regresa la lista sin el último elemento
ultimo :: [a] -> [a]
ultimo [] = []
ultimo [a] = []
ultimo (x:xs) = [x] ++ ultimo xs

-- Regresa el último elemento de una lista
ultimo2 :: [a] -> a
ultimo2 [a] = a
ultimo2 (x:xs) = ultimo2 xs

-- Obtiene los primero elementos n de una lista
obten :: [a] -> Int -> [a]
obten [] n = []
obten [a] 0 = []
obten (x:xs) n =  x: obten xs (n-1)

-- Contar el número de apariciones de un caracter en una lista
-- contar :: [Char] -> Int
contar []= 0
contar (a:xs) = if a == '1' then 1 + contar xs else contar xs

mindig (x:[]) = x
mindig (x:y:xs) = if x < y then mindig (x:xs) else mindig (y:xs)


inorden NIL = []
inorden (Arbol(x,t,y))=((inorden x)++[t])++(inorden y)

postorden NIL = []
postorden (Arbol(x,t,y)) = ((postorden x)++(postorden y))++[t]

-- elem1 :: -> a -> [a] -> Bool
elem1 n [] = False
elem1 n (x:xs) = if x == n then True else elem1 n xs

mist [] (n:ns) = []
mist (x:xs) (n:ns) = if elem1 x (n:ns) then mist xs (n:ns) else x:(mist xs (n:ns))
-- .........................................................................
-- (Arbol((Arbol((Arbol((Arbol(NIL,8,NIL)),4,(Arbol(NIL,9,NIL)))),2,(Arbol(NIL,5,NIL)))),1,(Arbol((Arbol(NIL,6,NIL)),3,(Arbol((Arbol(NIL,10,NIL)),7,NIL))))))
-- Desafio 05 ...........................................................

-- Regresa 2^n
f :: Int -> Int
f 0 = 1
f n = 2 * f(n-1)

-- Regresa la longitud de una lista
longitud :: [a] -> Int
longitud [] = 0
longitud (x:xs) = 1 + longitud xs

-- Regresa la suma de una lista
lista :: Num p =>[p] -> p
lista [] = 0
lista (x:xs) = x + lista xs

-- Regresa 2n +1
f3 :: Int -> Int
f3 0 = 1
f3 n = f3(n-1) + 2

-- Regresa la hoja más a la izquierda de un árbol
-- hmi :: Arbol -> Arbol
hmi NIL = NIL
hmi (Arbol(NIL,a,NIL)) = (Arbol(NIL, a, NIL))
hmi (Arbol(x,t,y))=hmi x

-- Cuenta el número de vértices de un árbol
nv :: Arbol -> Int
nv NIL = 0
nv (Arbol(x,t,y))=1 + nv x + nv y

-- Regresa la altura de un arbol
na :: Arbol -> Int
na NIL = 0
na (Arbol(x,t,y)) = 1 + max (na x) (na y)

-- Regresa una lista alréves
reversa :: [a] -> [a]
reversa [] = []
reversa (x:xs) = concat1 (reversa xs) [x]

-- Regresa si una lista es un palíndromo o no
-- palindromo :: [a] -> Bool
palindromo [] = True
palindromo [a] = True
palindromo (x:xs) = x == (ultimo2 xs) && palindromo (ultimo xs)

-- Potencia de un número
potencia2 :: Float -> Float -> Float
potencia2 0 _ = 0
potencia2 _ 0 =1
potencia2 x n = if par n then potencia2 (x*x) (n / 2) else x*potencia2 x (n-1)

-- Suma cada uno de los dígitos de un número
suma "" = 0
suma (n:ns) = casteo n + suma (ns)

--Regresa una lista con los vértices de un árbol si se recorre raíz, subárbol izquierdo, subárbol derecho
-- aplana :: Arbol -> [Int]
aplana NIL= []
aplana (Arbol(x,t,y)) = concat1 (concat1 [t] (aplana x)) (aplana y)

-- Obtiene el n-ésimo elemento de una lista
getNth :: Int -> [Int] -> Int
getNth _ [] = -1
getNth 0 (x:xs)= x
getNth n (x:xs) = getNth (n-1) xs

-- Regresa el elemento más pequeño de una lista de números
minimo :: [Int] -> Int
minimo [x] = x
minimo (x:y:xs) = if x > y then minimo(y:xs) else minimo(x:xs)

-- Regresa una lista con n veces el elemento m
repite 0 n = []
repite m n = n: (repite (m-1) n)
-- .........................................................................

-- Ejercicios Ramses -----------------------------------------------------

--Determina si el primer número es mayor que el segundo
mayor :: Int -> Int -> Bool
mayor 0 0 = False
mayor x 0 = True
mayor 0 y = False
mayor x y = mayor (x-1) (y-1)

--Determina si el primer número es menor que el segundo
menor :: Int -> Int -> Bool
menor 0 0 = False
menor x 0 = False
menor 0 y = True
menor x y = menor (x-1) (y-1)

-- División recursiva
division :: Int -> Int -> Int
division x n
            | n > x = 0
	    |otherwise = 1 +division (x-n) n

-- ELimina los elementos x de una lista
-- eliminar :: (Eq a) =>[a]-> a -> [a]
eliminar [] _ = []
eliminar (x:xs) a = if a == x then eliminar xs a else x: eliminar xs a

-- Agrega un elemento a un arbol
agrega NIL n = (Arbol(NIL,n,NIL))
agrega (Arbol(x,t,y)) n= (Arbol(agrega x n, t, copiar y))

-- Copia un arbol
copiar NIL = NIL
copiar (Arbol(x,t,y)) = (Arbol(copiar x, t, copiar y))

-- Determina si un elemento es un nodo de un árbol
pertenece ::  Arbol -> Int -> Bool
pertenece NIL n = False
pertenece (Arbol(x,t,y)) n = if n == t then True else if pertenece x n then True else if pertenece y n then True else False

-- Elimina los primeros elementos n de una lista
-- drop1 :: [a] -> Int -> [a]
drop1 [] n = []
drop1 (x:s) 0 = (x:s)
drop1 (x:xs) n = drop1 xs (n-1)

-- Toma los primeros n números de una lista
-- take1 :: [a] -> Int -> [a]
take1 [] n = []
take1 (x:xs) 0 = []
take1 (x:xs) n = x: take1 xs (n-1)

-- Crea una lista de tuplas donde el primer elemento es un elemento de la lista y el segundo el número de apariciones de dicho elemento
--frec1 :: [a] -> [(a,a)]
--frec1 [] = []
-- frec1 (x:xs) = (x , contar (x:xs) x): frec1 (eliminar xs x)

-- Calcula el doble factorial de un número
dobleF :: Int -> Int
dobleF 0 = 1
dobleF 1 = 1
dobleF n = n*dobleF(n-2)
