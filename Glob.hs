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
import GlobRegex (matchesRegex, unterminatedCharClassMessage)
import Control.Exception (handle)
import Control.Monad (forM)

namesMatching :: String -> IO [String]
namesMatching pattern 
	| not $ isPattern pattern = do
		exists <- doesNameExist pattern
		return (if exists then [pattern] else [])
	| otherwise = do
		case splitFileName pattern of
			("", baseName) -> do
				curDir <- getCurrentDirectory
				listMatches curDir baseName
			(dirName, baseName) -> do
				dirs <- if isPattern dirName
						then namesMatching $ dropTrailingPathSeparator dirName
						else return [dirName]
				let listDir = if isPattern baseName
							then listMatches
							else listPlain
				pathNames <- forM dirs $ (\dir -> do
									baseNames <- listDir dir baseName
									return (map (dir </>) baseNames))
				return (concat pathNames)

isPattern :: String -> Bool
isPattern text = any (\e -> e `elem` patternSymbols) text
patternSymbols = "?[*"

doesNameExist :: FilePath -> IO Bool
doesNameExist path = do
	file <- doesFileExist path 
	if file then
		return (True)
	else
		doesDirectoryExist path 

listMatches = undefined

listPlain = undefined