module Glob
(
	namesMatching
)

where 

import System.Directory (doesDirectoryExist, doesFileExist,
	getCurrentDirectory, getDirectoryContents)
import System.FilePath (dropTrailingPathSeparator, 
	splitFileName, (</>))
import Utils
import GlobRegex (matchesRegex, unterminatedCharClassMessage, matchesRegexCaseSencitive)
import Control.Exception (handle)
import Control.Monad (forM)
import System.FilePath (pathSeparator)

namesMatching :: String -> IO [String]
namesMatching pattern 
	| not $ isPattern pattern = do
		exists <- doesNameExist pattern
		return (if exists then [pattern] else [])
	| not. null $ fst $ findTwoStars pattern = undefined 
	| otherwise = do
		case splitFileName pattern of
			("", baseName) -> do
				curDir <- getCurrentDirectory
				listMatches curDir baseName
			(dirName, baseName) -> do
				dirs <- if isPattern dirName
						then namesMatching $ dropTrailingPathSeparator dirName
						else return [dirName]
				let listDir = if isPattern baseName -- listDir is a function declaration
							then listMatches
							else listPlain
				pathNames <- forM dirs $ (\dir -> do
									baseNames <- listDir dir baseName
									return (map (dir </>) baseNames))
				return (concat pathNames)

isPattern :: String -> Bool
isPattern text = any (\e -> e `elem` patternSymbols) text
	where patternSymbols = "?[*"

getTypeSencitivity :: Bool
getTypeSencitivity 
	| pathSeparator == '\\' = False
	| pathSeparator == '/' = True
	| otherwise = error "Unknown Operation System" 

doesNameExist :: FilePath -> IO Bool
doesNameExist path = do
	file <- doesFileExist path 
	if file then
		return (True)
	else
		doesDirectoryExist path 

listMatches :: FilePath -> String -> IO [FilePath]
listMatches dir pattern = do
	dirName <- if null dir
				then getCurrentDirectory
				else return (dir)
	handle handleException  $ do
		content <- getDirectoryContents dirName
		let names = if isHidden pattern
						then filter isHidden content
						else filter (not . isHidden) content
		return (filter (\e -> matchesRegex e pattern $ getTypeSencitivity) names)
		
handleException :: IOError -> IO [a]
handleException e = const (return []) e
		
isHidden ('.':_) = True
isHidden _ = False
				
listPlain :: FilePath -> String -> IO [FilePath]
listPlain dir baseName = do
	exists <- if null baseName
				then doesDirectoryExist dir
				else doesNameExist (dir </> baseName)
	return (if exists then [baseName] else [])

findTwoStars :: String -> (String, String)
findTwoStars s = findTwoStars' "" s

findTwoStars' :: String -> String -> (String, String)
findTwoStars' prev ('*':'*':rest) = (prev ++ "*", rest)
findTwoStars' prev (c:cs) = findTwoStars' (prev ++ [c]) cs
findTwoStars' prev [] = (prev, [])
