module NoteParser (parseLines) where
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
-- Lists
parseLines (('#':'l':'i':'s':'t':_):css) =  (map (\line -> "* " ++ line) (takeWhile stopFunct css)) ++
						(parseLines $ tail $ (dropWhile stopFunct css))
						where
						stopFunct = (\line -> (trim line) /= "#endlist")
-- Code Sections -- RST requires a newline either side of the ".. code:: "
parseLines (('#':'c':'o':'d':'e':cs):css) = [] : ( ".. code:: " ++ (trim cs)) : [] :
						((map indent (takeWhile stopFunct css)) ++ 
						(parseLines $ tail $ (dropWhile stopFunct css)))
						where
						indent :: [Char] -> [Char]
						indent s = '\t' : s 
						stopFunct = (\line -> (trim line) /= "#endcode")
--default
parseLines (cs:css) = cs : (parseLines css)
