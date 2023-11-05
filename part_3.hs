import Data.Char

---- Part 3 ----

--swapwords calles the function findword with inputs of a list of words in the sentence s and 
--w1 (word to be found and replaced) and w2 (the word to replace instances of w1 with).
--'words s' converts the sentence s into a list of its constituent words.

swapwords :: String -> String -> String -> String
swapwords w1 w2 s = findword (words s) w1 w2

--findword finds the words in the list of words from sentence s that have a part of or the whole of w1
--(the word to be replaced) in them, the function that finds these words is wordinword that returns a 
--boolean value. If a word contains w1, then  shangeword is called on that word to change part of or the 
--whole of the word. Then, the changed or unchanged word is concatinated with the output of findword on 
--the rest of the sentence.

findword :: [String] -> String -> String -> String
findword (s:ss) w1 w2
	|ss == []					= s
 	|wordinword s w1 == True 	= changeword w1 w2 s ++ " " ++ findword ss w1 w2
	|wordinword s w1 == False	= s ++ " " ++ findword ss w1 w2
	
--Changeword concatinates w2 (the word to replace with) to the word in the sentence that has w1 (the
--word to be replaced) in it. But delete is first called on s (the word that is being changed), to remove
--the part that contains w1 in it.
		
changeword :: String -> String -> String -> String
changeword w1 w2 s = w2 ++ (delete w1 s)

--delete removes all characters from s:ss that match w1:w1s, to remove the word from it.
	
delete :: String -> String -> String
delete [] ss = ss
delete w1s [] = w1s
delete (w1:w1s) (s:ss)
	|s == w1			="" ++ delete w1s ss
	|s /= w1 			=s:ss

--wordinword checks if the first or all characters of a word match the characters of w1.
--if yes, it returns True, if not, it returns False. 

wordinword :: String -> String -> Bool
wordinword (s:ss) (w:ws)
	|ws == []			= True
	|ss == []			= False
	|w == s				= wordinword ss ws
	|w /= s				= False
