module GlobRegex
(
	globToRegex,
	matchesRegex
)
where 

import qualified Text.Regex.Posix ((=~))
import Utils

globToRegex :: String -> String
globToRegex cs = '^' : globToRegexHelper cs ++ "$"

matchesRegex :: String -> String -> Bool
matchesRegex _ _ = undefined

globToRegexHelper :: String -> String
globToRegexHelper "" = ""

globToRegexHelper ('*':cs) = ".*" ++ globToRegexHelper cs
globToRegexHelper ('?':cs) = "." ++ globToRegexHelper cs

globToRegexHelper ('[':'!':c:cs) = "[^" ++ c : charClass cs
globToRegexHelper ('[':c:cs) = "[" ++ c : charClass cs
globToRegexHelper ('[':_) = error "unterminated character class"

globToRegexHelper (c:cs) = escape c ++ globToRegexHelper cs

