module Utils
(
	clearLinux,
	clearWindows
)
where
{-# LANGUAGE TypeSynonymInstances, OverlappingInstances #-}

import System.IO
import System.Process
import System.Random

clearLinux = system "clear"

clearWindows = system "cls"
