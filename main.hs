module Main where
import Utils

main = do
	content <- getContents
	mapM_ putStrLn $ convertToRST $ lines $content	

convertToRST :: [[Char]] -> [[Char]]
convertToRST = parseLines . map trim
	where
	parseLines :: [[Char]] -> [[Char]]
	-- Base case
	parseLines [] = []
	-- Title
	parseLines (('>':cs):css) = (listOf '=' len) : (trim cs) : (listOf '=' len) : parseLines css
		where len = length cs
	-- Header
	parseLines (('=':'>':cs):css) = (listOf '-' len) : (trim cs) : (listOf '-' len) : parseLines css
		where len = length cs
	--default
	parseLines (cs:css) = cs : parseLines css
