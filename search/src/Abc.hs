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

import Domain

abcConstraint :: String -> Int -> Constraint
abcConstraint s n d = let (min,max) = eval s d in
    if min > n || max < n then [] else d

value :: Char -> Domain -> Int
value = undefined

eval :: String -> Domain -> (Int, Int)
eval s d = foldr (eval' d) (0,0) s

eval' d c (min,max) = let (m1,m2) = limits c d in (m1+min,m2+max)

limits c d = let dom = domain c d in (head dom, final dom)

domain 'a' (v:_) = v
domain c d = domain (pred c) (tail d)


final :: [a] -> a
final (x:[]) = x
final (_:xs) = final xs

-- sum = w1 * v1 + w2 * v2 + w3 * v3
-- all w positive
sumConstraint :: Int -> [Int] -> Constraint
sumConstraint = undefined

propagate :: Int -> [Domain] -> [Domain]
propagate total domains = map limitDomain (zip domains (reduce total (boundaries domains)))

limitDomain :: Domain -> (Int,Int) -> Domain
limitDomain d (min,max) = undefined

boundaries :: [Domain] -> [(Int, Int)]
boundaries domains = map (\d -> (head d, final d)) domains

reduce :: Int -> [(Int, Int)] -> [(Int,Int)]
reduce total constraints = let
    upper = sum $ map snd constraints
    lower = sum $ map fst constraints
    in
    map (\d -> (max (fst d) (total - (upper - snd d)), min (snd d) (total - (lower - fst d)))) constraints


-- a + b + c = 10 ; a:[1..5], b:[3..7], c[2..4]
-- lowerLimit = 6
-- upperLimit = 16
-- lowerLimits = [10-(16-5),10-(16-7),10-(16-4)] = [-1,1,-2]
-- upperLimits = [10-(6-1),10-(6-3),10-(6-2)] = [5,7,6]
