module Utils
(
	clear,
        splitOn,
        deleteAll
)
where
{-# LANGUAGE TypeSynonymInstances, OverlappingInstances #-}

import System.IO
import System.Process
import System.Random

clear = system "clear"

splitOn :: String -> String -> [String]
splitOn src symbols = 
                    deleteAll (splitHelper [] src) []
                     where
                        len = length symbols

                        splitHelper :: [String] -> String -> [String]
                        splitHelper res [] = reverse (map reverse res)
                        splitHelper [] (c:cs) = splitHelper [[c]] cs
                        splitHelper (curr:ready) str 
                                        | length str < length symbols = splitHelper ((str++curr):ready) []
                                        | cut /= symbols = let lead = ((head str):curr)
                                                           in splitHelper (lead:ready) (tail str)
                                        | cut == symbols = splitHelper ([]:(curr:ready)) (drop len str)
                                        where cut = take len str

deleteAll :: (Eq a) => [a] -> a -> [a]
deleteAll [] _ = []
deleteAll (e:es) m 
        | m == e = deleteAll es m
        | otherwise = e : (deleteAll es m)
