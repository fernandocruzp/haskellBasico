f :: Int -> Int -> Int
f x y = x + y

g :: Bool -> Bool
g b = not b

conj :: Bool -> Bool -> Bool
conj p q = p && q

parabola :: Int -> Int
parabola x = x*x + 7*x +8

h :: Int -> Int
h x = parabola x  +2

factorial :: Int -> Int 
factorial = if x <= 0
	  then 1
	  else x * factorial (x - 1)

gauss :: Double -> Double
gauss x = (x *(x+1)) / 2

plano :: Int -> Int -> (Int, Int)
plano x y = (x,y)

{-inter100 :: (Ord a, Num a) => a -> Bool
inter100 n = if n >= 1
	     then if n<=100
	     	  then True
	     	  else False
	     else False

yeih :: (Ord a, Num a) => a -> [Char]
yeih n = if inter100 n
       	 then "Yeoh"
	 else "ajsjsj"	

-}