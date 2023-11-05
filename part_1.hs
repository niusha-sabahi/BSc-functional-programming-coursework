import Data.Char

---- Part 1 ----

--This part prints steps of height m, width n and number p. 
--The function line prints lines of stars of length n by recursively concatinating them. 

line :: Int -> String
line n 
    |n > 0				= "*" ++ line(n-1)
    |otherwise 			= ""
	
--The line function is called
--by the square function which prints the individual
--blocks (width n and height m) that make up the steps, e.g. for step 1, there's one 
--block and for step 2 there's 2 and so on...
--These blocks are recursively concatinated.

square :: Int -> Int -> String
square m n
    |m > 0				= line(n) ++ "\n" ++ square (m-1) n
    |otherwise			= ""

--Finally, the steps function calls the square function for the 
--relevant number of times for that step (as per example given above)
--to give steps that are succesively longer by one block.

--It also prints "" if the input for width (n) is 0. This is the second line.
	
steps :: Int -> Int -> Int -> String
steps m 0 p = ""
steps m n p 
    |p > 0				= steps m n (p-1) ++ square m (n*p)
    |otherwise			= ""