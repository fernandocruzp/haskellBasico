
funcionVariables n =
   let
     m = n + 1
   in
     m

funcionVariablesW n = m
  where
      m = sucesor n
      sucesor :: Int -> Int
      sucesor n = n + 1

f' 0 y z = y * z
f' x 3 z = z - x
f' x y 8 = x / y
f' x y z = (x + y) - z

g' x y
   | x == y = "Son iguales"
   | x > y = "X es más grande"
   | otherwise = "Y es más grande"

estoyGordo :: Double -> Double -> String
estoyGordo m h
   | imc m h >= 30 = "estoy obeso"
   | imc m h > 25 = "Estás gordo"
   | imc m h >= 19 = "estas bien"
   | otherwise = "estas desnutrido"
   where
       imc m h = m / (h*h)

h' x y = case (odd x) of
       	  True -> y
	  False -> x

data Primarios = Rojo | Amarillo | Azul deriving (Show)

data Secundario = Verde | Naranja | Morado deriving (Show)

mezcla :: Primarios -> Primarios -> Secundario
mezcla Rojo Amarillo = Naranja
mezcla Rojo Azul = Morado

miReversa :: [a] -> [a]
miReversa [] = []
miReversa (x:xs) = miReversa xs ++ [x]