--Cruz Pineda Fernando
--423076479

areaCirc :: Float -> Float
areaCirc x = pi * x*x

distanciaPuntos :: (Float,Float) -> (Float,Float) -> Float
distanciaPuntos (x,y) (z,t) = sqrt((x-z)*(x-z)+(y-t)*(y-t))


incremental :: Int -> Int -> Int -> Int -> Bool
incremental x y z t =
	      	    if x <= y
		       then
			   if y <= z
			      then
				if z <= t
				   then True
				else False
			    else False
		    else False

sumaGauss :: Int -> Int
sumaGauss 0 = 0
sumaGauss x = x + sumaGauss(x-1)


areaTri :: (Float,Float) -> (Float,Float) -> (Float,Float) -> Float
areaTri (x,y) (z,t) (u,v) = sqrt(s * (s-a) * (s-b) * (s-c))
	      	    	  where s = (a + b  + c )/2
			  	a= distanciaPuntos (x,y) (z,t)
			  	b= distanciaPuntos (x,y) (u,v)
			  	c = distanciaPuntos (u,v) (z,t)
	      	    	  

hipotenusa :: Float -> Float -> Float
hipotenusa x y = sqrt(x*x + y*y)

normaVectorial :: (Float,Float) -> Float
normaVectorial (x,y) = distanciaPuntos (0,0) (x,y)

volumen :: Float -> Float
volumen x = (pi*x*x*x)*4/3

comparador :: Float -> Float -> Int
comparador x y
	 | x == y = 0
	 | x > y  = 1
	 | otherwise = -1

sumaFraciones :: (Int,Int) -> (Int,Int) -> (Int,Int)
sumaFraciones (x,y) (u,v) = (x*v + y*u ,y*v)

productoPunto :: (Float,Float) -> (Float,Float) -> Float
productoPunto (x,y) (v,u) = x*v + y*u

pendienteRecta :: (Float,Float) -> (Float,Float) -> Float
pendienteRecta (x,y) (v,u) = (u-y)/(v-x)

data PiezasAjedrez = Peon | Caballo | Alfil | Torre | Reina | Rey deriving (Show)

primerMovimiento :: PiezasAjedrez -> String -> Bool
primerMovimiento Reina x = False
primerMovimiento Torre x = False
primerMovimiento Alfil x = False
primerMovimiento Rey x = False
primerMovimiento Caballo "A3" = True
primerMovimiento Caballo "C3" = True
primerMovimiento Caballo "F3" = True
primerMovimiento Caballo "H3" = True
primerMovimiento Caballo "A6" = True
primerMovimiento Caballo "C6" = True
primerMovimiento Caballo "F6" = True
primerMovimiento Caballo "H6" = True
primerMovimiento Caballo x = False
primerMovimiento Peon "A4" = True
primerMovimiento Peon "A3" = True
primerMovimiento Peon "A6" = True
primerMovimiento Peon "A5" = True
primerMovimiento Peon "B3" = True
primerMovimiento Peon "B4" = True
primerMovimiento Peon "B6" = True
primerMovimiento Peon "B5" = True
primerMovimiento Peon "C3" = True
primerMovimiento Peon "C4" = True
primerMovimiento Peon "C6" = True
primerMovimiento Peon "C5" = True
primerMovimiento Peon "D3" = True
primerMovimiento Peon "D4" = True
primerMovimiento Peon "D6" = True
primerMovimiento Peon "D5" = True
primerMovimiento Peon "E3" = True
primerMovimiento Peon "E4" = True
primerMovimiento Peon "E6" = True
primerMovimiento Peon "E5" = True
primerMovimiento Peon "F3" = True
primerMovimiento Peon "F4" = True
primerMovimiento Peon "F6" = True
primerMovimiento Peon "F5" = True
primerMovimiento Peon "G3" = True
primerMovimiento Peon "G4" = True
primerMovimiento Peon "G6" = True
primerMovimiento Peon "G5" = True
primerMovimiento Peon "H3" = True
primerMovimiento Peon "H4" = True
primerMovimiento Peon "H6" = True
primerMovimiento Peon "H5" = True
primerMovimiento Peon x = False

