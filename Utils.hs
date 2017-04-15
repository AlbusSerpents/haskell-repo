module Utils
(
	clear
)
where
{-# LANGUAGE TypeSynonymInstances, OverlappingInstances #-}

import System.IO
import System.Process
import System.Random

clear = system "clear"
