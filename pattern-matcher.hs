module GlobRegex
(
	globToRegex,
	matchesRegex, 
	unterminatedCharClassMessage
)
where 

import Text.Regex.Posix ((=~))
import Utils

globToRegex :: String -> String
globToRegex cs = '^' : globToRegexHelper cs ++ "$"

matchesRegex :: String -> String -> Bool
name `matchesRegex` pat = name =~ globToRegex pat

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