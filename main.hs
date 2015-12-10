module Main where
import Utils

main = do
	content <- getContents
	mapM_ putStrLn $ convertToRST $ lines $content	

convertToRST :: [[Char]] -> [[Char]]
convertToRST = parseLines . map trim

parseLines :: [[Char]] -> [[Char]]
-- Base case
parseLines [] = []
-- Title
parseLines (('>':cs):css) = (listOf '=' (length cs)) : (trim cs) : (listOf '=' (length cs)) : parseLines css
-- Header
parseLines (('=':'>':cs):css) = (listOf '-' (length cs)) : (trim cs) : (listOf '-' (length cs)) : parseLines css
-- section headings
parseLines (('-':'>':cs):css) = (trim cs) : (listOf '-' (length cs)) : parseLines css
-- Subsection headings
parseLines (('-':'-':'>':cs):css) = (trim cs) : (listOf '~' (length cs)) : parseLines css
-- Code Sections
parseLines (('#':'c':'o':'d':'e':cs):css) = (".. code:: " ++ (trim cs)) : multiLineApplyFunction (\line -> "\t" ++ line)
				 (\line -> (trim line) == "#endcode") css
--default
parseLines (cs:css) = cs : (parseLines css)


-- Function to change modify string passed -> Function to check whether to stop or not -> Lines 
-- Returns the modified lines                                                                                    
multiLineApplyFunction :: ([Char] -> [Char]) -> ([Char] -> Bool) -> [[Char]] -> [[Char]] 
multiLineApplyFunction f checkToStop (cs:css)
				| ((checkToStop cs) == True) = parseLines css
				| otherwise = (f cs) : (multiLineApplyFunction (f) (checkToStop) css)
