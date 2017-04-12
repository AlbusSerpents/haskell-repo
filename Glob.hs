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

isPattern :: String -> Bool
isPattern text = any (\e -> e `elem` patternSymbols) text

patternSymbols = "?[*"

doesNameExist = undefined