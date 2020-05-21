-- Chrysanthi Monastirli A.M. 1716 

-----------------------------------------------------------------------------------------
-- ASKHSH 1 

statistics :: [(Int,Int)]->(Int,Int,Int,Int,Int)
statistics list = if length list >0 then (length list, points list, scoredSum list, takenSum list, best list) else (0, 0, 0, 0, 0)

active	::		(Int,Int)->(Int)
active	(a,p)	= a

passive	::	(Int,Int)->(Int)
passive	(a,p)	= p

scoredSum	::		[(Int,Int)]->(Int)
scoredSum	[] 		= 0
scoredSum	list	= active (head list) + scoredSum (tail list)

takenSum	::		[(Int,Int)]->(Int)
takenSum	[]		= 0
takenSum	list	= passive (head list) + takenSum (tail list)

points	::			[(Int,Int)]->(Int)
points	[]			= 0
points	list	| active (last list) == passive (last list) = points (init list) + 1
				| active (last list) > passive (last list)	= points (init list) + 3 
				| otherwise 								= points (init list)
					
best ::	[(Int,Int)]->(Int)
best (k:[])	= active k - passive k
best list 	| dif >= best (tail list) 	= dif
			|otherwise 					= best (tail list)
			where dif = active (head list) - passive (head list)
			
-----------------------------------------------------------------------------------------
-- ASKHSH 2 

wordList 	:: 		String->[String]
wordList 	str 	=if length str == 0 then [] else if isAlpha (head str) then [word (str)] ++ findNext (tail str) else wordList (tail str)

word		::		String->String
word		str		= 	if length str == 1 then 
							if isAlpha (head str) then [head str] else []
						else if isAlpha (head str) then [head str] ++ word (tail str) 
						else []

findNext	::		String->[String]
findNext	str		= 	if length str == 1 || length str ==0 then []
						else if isAlpha (head str) then findNext (tail str)
						else wordList (tail str)

isAlpha 	:: 		Char -> Bool
isAlpha 	ch 		= if (ch>='A' && ch<='Z') || (ch>='a' && ch<='z') then True else False

-----------------------------------------------------------------------------------------
-- ASKHSH 3 

move :: Eq u => [u]->u->Int->[u]
move (k:[]) x n = if (n == 0 && (x==k)) then [] else (k:[])
move s x n = checkExist s s x n 0

checkExist :: Eq u => [u]->[u]->u->Int->Int->[u]
checkExist [] sR x n index = sR
checkExist s sR x n index 	| (head s) /= x = checkExist (tail s) sR x n (index+1)
							| otherwise = move2 sR x n index

move2 :: Eq u => [u]->u->Int->Int->[u]
move2 s x n index 	| n > 0 = (take index s) ++ take n (drop (index+1) s) ++ [x] ++ drop (n+index+1) s
					| n < 0 = 	if n+index > 0 then (take (index+n) s) ++ [x] ++ take (-n) (drop (index+n) s) ++ drop (1+index) s
								else [x] ++ take index s ++ drop (1+index) s
					| otherwise = take index s ++ drop (1+index) s

-----------------------------------------------------------------------------------------
 -- ASKHSH 4 

integral :: (Double -> Double) -> Double-> Double-> Double-> Double
integral f a b d	| b-a<=d	= (f ((a+b)/2))*(b-a)
					| otherwise		= integral f a ((a+b)/2) d + integral f ((a+b)/2) b d

-----------------------------------------------------------------------------------------
 -- ASKHSH 5                                   

hof	::	[Integer]->(Integer->Integer)
hof	[]	= \n ->		0
hof	s	= \n ->		if (head s) == n then 1 else (hof2 (tail s) 2) n

hof2 :: [Integer]->Integer->(Integer->Integer)
hof2 [] _ = \n -> 0
hof2	s c	= \n ->		if (head s) == n then c else (hof2 (tail s) (c+1) ) n