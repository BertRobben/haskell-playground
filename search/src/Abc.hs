-----------------------------------------------------------------------------
--
-- Module      :  Abc
-- Copyright   :
-- License     :  AllRightsReserved
--
-- Maintainer  :
-- Stability   :
-- Portability :
--
-- |
--
-----------------------------------------------------------------------------

module Abc (

) where

import Constraint.AllDifferent
import Constraint.Sum
import SearchSpace
import Data.Char

exampleAbc :: SimpleSearchSpace Int
exampleAbc = abcSearchSpace [("bonobo",44), ("condor",69), ("doejong",78),("gaviaal",142),
    ("gibbon",51), ("gnoe",40), ("huismus",54), ("lemming",66), ("makaak",100), ("maki", 39),
    ("narwal",127), ("oryx", 47), ("python", 60), ("quetzal", 118), ("zeelt",97)]

abcSearchSpace :: [(String, Int)] -> SimpleSearchSpace Int
abcSearchSpace cons = simpleSearchSpace
    (map (\c -> (show c,[1..26])) ['a'..'z']) -- variables
    (((map show ['a'..'z']),newConstraint allDifferent "diff"):(map (\(s,n) -> abcConstraint s n) cons))

abcConstraint :: String -> Int -> ([String], Constraint Int)
abcConstraint s n = let cc = countCharacters s in
    (map (show . fst) cc,newConstraint (sumConstraint n $ map snd cc) s)

countCharacters :: String -> [(Char,Int)]
countCharacters s = foldl (\res c -> addCharacter c res) [] s

addCharacter :: (Eq a) => a -> [(a,Int)] -> [(a,Int)]
addCharacter a [] = [(a,1)]
addCharacter a ((c,n):rest) = if (c == a) then (c,n+1):rest else (c,n):(addCharacter a rest)
