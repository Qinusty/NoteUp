module Utils where

trim :: [Char] -> [Char]
trim = reverse . removeTrailing ' ' . reverse . removeTrailing ' '
	where
	removeTrailing :: Char -> [Char] -> [Char]
	removeTrailing _ [] = []
	removeTrailing a (c:cs) 
		| (a == c) = removeTrailing a cs
		| otherwise = (c:cs)

listOf :: Char -> Int -> [Char]
listOf _ 0 = []
listOf c i = c : listOf c (i-1)

