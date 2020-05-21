-- Chysanthi Monastirli A.M. 1716  --
-----------------------------------------------------------------------------------------

-- ASKHSH 1

amount :: Int->Float->Float
amount n p
	| n < 0 || p < 0 									= 0
	| n <=4		&& 	count*p <= 100 						= count * p
	| n <=4 	&& 	count*p > 100						= count*p*0.9
	| n <=8 	&& (count-1)*p <= 100					= (count-1)*p
	| n <=8 	&& (count-1)*p > 100					= (count-1)*p*0.9
	| n <=11 	&& (count-2)*p <= 100					= (count-2)*p
	| n <=11 	&& (count-2)*p > 100					= (count-2)*p*0.9
	| (count-2- fromIntegral((n-9) `div` 3)) *p <=100	= (count-2- fromIntegral((n-9) `div` 3)) *p
	| otherwise 										= (count-2- fromIntegral((n-9) `div` 3)) *p*0.9
	where count = fromIntegral n

-----------------------------------------------------------------------------------------

-- ASKHSH 2

cost :: (Int,Int,Int)->(Int,Int,Int)->Float
cost (h1,m1,s1) (h2,m2,s2)
	| dif == 0.0 	= 0
	| dif <= 180 	= 0.58
	| otherwise 	= 0.58 + 0.003*(dif-180)
	where dif
		| dif2 >=0 	= dif2
		| otherwise = dif2 + 24*3600
		where dif2 	= fromIntegral (h2*3600 + m2*60 + s2 - h1*3600 - m1*60 - s1)

-----------------------------------------------------------------------------------------

-- ASKHSH 3

cp :: Integer->Integer
cp n = cp2 n 0

cp2 :: Integer->Integer->Integer
cp2 n i
	| abs (2^i - n) < abs (2^(i+1) - n) 	= 2^i
	| otherwise 							= cp2 n (i+1)

-----------------------------------------------------------------------------------------

-- ASKHSH 4

compress :: Integer->Integer
compress n = compress2 n 1

compress2 :: Integer->Integer->Integer
compress2 n i
	| n == 0 && i `div` 10 == 0 = i
	| n == 0 && i `div` 10 /= 0 = compress2 i 1
	| n `mod` 10 == 0 			= compress2 (n `div` 10) i
	| otherwise 				= compress2 (n `div` 10) (i * (n `mod` 10))

-----------------------------------------------------------------------------------------

-- ASKHSH 5

ab :: Int->(Int,Int)
ab n = ab2 n (1,n) (1,n)

ab2 :: Int->(Int,Int)->(Int,Int)->(Int,Int)
ab2 n (a,b) (x,y)
	| b == 0 					= (x,y)
	| a == b+1 					= ab2 n (1, b-1) (x, y)
	| a*b == n && b-a < y-x 	= ab2 n (a+1, b) (a,b)
	| otherwise 				= ab2 n (a+1, b) (x, y)
