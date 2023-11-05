import Data.Char

---- Part 2 ----

--This part will print a flag pattern of dimentions n.

--The main function, falgpattern, creates different flagpatters for even and odd 
--dimensions.

--For both odd and even inputs, the line2 function (which functions in the same way as the
--line function in part 1) will print the top and bottom lines of the flag.

--However, for even inputs the upwardsVeven is called and then downwardsV (which is used for both
--odd and even dimensions). Whereas for odd inputs, upwardsVodd is called, then oddMiddleLine and then downwardsV.
--This is because the centers of odd and even flags are different.

flagpattern :: Int -> String
flagpattern n 
    |n `mod` 2 == 0 		= line2 n ++ "\n" ++ upwardsVeven (n-2) 0 ++ downwardsV (n-2) ((n `div` 2)-2) ++ line2 n ++ "\n"
    |n `mod` 2 /= 0			= line2 n ++ "\n" ++ upwardsVodd (n-2) 0 ++ oddMiddleLine (n-2) ++ downwardsV (n-2) ((n `div` 2)-2) ++ line2 n ++ "\n" 
	
--spaces creates a string of blank spaces of length s by recursively concatinating them.
	
space :: Int -> String
space s 
    |s > 0 				= " " ++ space (s-1)
    |otherwise			= ""

--as mentioned, line2 creates a string of stars of length n.

line2 :: Int -> String
line2 n 
    |n > 0				= "*" ++ line2(n-1)
    |otherwise 			= ""
	
--upwardsVeven recursively makes the the lines between the top row of stars and
--the middle of the even flagpatterns.

--m is the width of the flag minus the stars on either side and its value is constant
--in the recursion. c is the counter and starts at 0, it increases by 1 in each 
--recusion to give bigger spaces on either sides in each succesive line (and succesively smaller
--spaces in the middle of the two stars that make the diagonal).
--It prints a star, blank spaces of size c and then another star for the leading diagonal,
--then it prints spaces of size m-2*c-2 (this formula gives a number that is 2 smaller each time).
--Finally, it prints the star for the other diagonal and then spaces c and a star for the other side.

--The function stops when c is smaller than m - (m `div` 2).

upwardsVeven :: Int -> Int -> String
upwardsVeven m c 
    |c < m - (m `div` 2)	= "*" ++ space c ++ "*" ++ space (m-2*c-2) ++ "*" ++ space c ++ "*\n" ++ upwardsVeven m (c+1)
    |otherwise				= ""
	
--downwardsV recursively makes the the lines between the middle and
--the bottom row of stars flagpatterns.

--downwardsV works similarly to upwardsVeven, but the initial value of c is (n `div` 2)-2
--as thats the number of spaces on either side of the middle stars in the fisrt line in the 
--downwards V shape of the flag in both odd and even flags. It also reduces the value of c by one 
--in each recursion rather than increasing it by one, which also means it stops when c = 0.
	
downwardsV :: Int -> Int -> String
downwardsV m c 
    |c >= 0					= "*" ++ space c ++ "*" ++ space (m-2*c-2) ++ "*" ++ space c ++ "*\n" ++ downwardsV m (c-1)
    |otherwise				= ""
	
--upwardsVodd is also very similar to upwardsVeven, the difference is that it stops printing lines
--sooner, when c = (m `div` 2)-1

upwardsVodd :: Int -> Int -> String
upwardsVodd m c
    |c <= (m `div` 2) - 1		= "*" ++ space c ++ "*" ++ space (m-2*c-2) ++ "*" ++ space c ++ "*\n" ++ upwardsVodd m (c+1)
    |otherwise					= ""

--oddMiddleLine prints only the line between the upwards and downwards Vs for odd flags which only have 1 star in the middle.
--it prints a star, then spaces of size m `div` 2 on eitehr side of the central star and then the star on the other side.

oddMiddleLine :: Int -> String
oddMiddleLine m = "*" ++ space (m `div` 2) ++ "*" ++ space (m `div` 2) ++ "*\n"