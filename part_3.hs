import Data.Char

---- Part 4 ----
	
compatibility :: String -> String -> String
compatibility name1 name2 = lphi name1 name2 (g name1 name2) ++ " and " ++ lphi name2 name1 (g name2 name1)

g :: String -> String -> String
g x (y:[]) = f x y  
g x (y:ys) = g (f x y) ys 

f :: String -> Char -> String
f [] y = ""
f (x:xs) y 
	|x == y 		= "" ++ xs 
	|x /= y			= x:f xs y

lphi :: String -> String -> String -> String
lphi name1 name2 name1fg
	|relate == 1		= name1 ++ " loves " ++ name2
	|relate == 2		= name1 ++ " is physical towards  " ++ name2
	|relate == 3		= name1 ++ " hates " ++ name2
	|relate == 0		= name1 ++ " is indifferent to " ++ name2
	where relate = length name1fg `mod` 4
