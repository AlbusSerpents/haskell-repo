module GlobRegex
(
	globToRegex,
	matchesRegex, 
	unterminatedCharClassMessage,
	matchesRegexCaseSencitive,
)
where 

import Text.Regex.Posix ((=~))
import Data.Char (toUpper)
import Data.Either

type GlobError = String

globToRegex :: String -> String 
globToRegex cs = '^' : globToRegexHelper cs ++ "$"

matchesRegexCaseSencitive :: String -> String -> Bool
matchesRegexCaseSencitive src pat = matchesRegex src pat True

matchesRegex :: String -> String -> Bool -> Bool
matchesRegex src pat False = 
                        iSrc =~ (globToRegex iPat)
                                where 
		                        iSrc = map toUpper src
			                iPat = map toUpper pat
matchesRegex src pat True = src =~ (globToRegex pat)

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
