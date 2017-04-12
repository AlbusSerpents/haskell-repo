module GlobRegex
(
	globToRegex,
	matchesRegex, 
	unterminatedCharClassMessage
)
where 

import Text.Regex.Posix ((=~))
import Data.Char (toUpper)
import Utils

globToRegex :: String -> Bool -> String 
globToRegex cs False = '^' : globToRegexHelper insensitive ++ "$"
						where insensitive = map toUpper cs
globToRegex cs True = '^' : globToRegexHelper cs ++ "$"

matchesRegex :: String -> String -> Bool -> Bool
matchesRegex src pat False = iSrc =~ (globToRegex iPat False)
								where 
									iSrc = map toUpper src
									iPat = map toUpper pat
matchesRegex src pat True = src =~ (globToRegex pat False)

globToRegexHelper :: String -> String
globToRegexHelper "" = ""

globToRegexHelper ('*':cs) = ".*" ++ globToRegexHelper cs
globToRegexHelper ('?':cs) = "." ++ globToRegexHelper cs

globToRegexHelper ('[':'!':c:cs) = "[^" ++ c : charClass cs
globToRegexHelper ('[':c:cs) = "[" ++ c : charClass cs
globToRegexHelper ('[':_) = error unterminatedCharClassMessage

globToRegexHelper (c:cs) = escape c ++ globToRegexHelper cs

escape :: Char -> String
escape character
	| character `elem` escapeSymbols = '\\' : character : []
	| otherwise = [character]
	where escapeSymbols = "\\+()^%$.{}]|"

charClass :: String -> String
charClass (']':cs) = ']' : globToRegexHelper cs
charClass (c:cs) = c : charClass cs
charClass [] = error unterminatedCharClassMessage

unterminatedCharClassMessage = "unterminated character class"