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
abcConstraint s n d = undefined

value :: Char -> Domain -> Int
value = undefined

eval :: String -> Domain -> (Int, Int)
eval s d = undefined

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
sumConstraint total weights domains = map limitDomain (zip domains (reduce total weights (boundaries domains)))

limitDomain :: (Domain,(Int,Int)) -> Domain
limitDomain ([],_) = []
limitDomain ((d:ds),c@(min,max)) = if d < min then limitDomain (ds,c) else
    if d > max then [] else d : (limitDomain (ds,c))

boundaries :: [Domain] -> [(Int, Int)]
boundaries domains = map (\d -> (head d, final d)) domains

reduce :: Int -> [Int] -> [(Int, Int)] -> [(Int,Int)]
reduce total weights constraints = let
    upper = weightedSum weights $ map snd constraints
    lower = weightedSum weights $ map fst constraints
    in
    map (\(w,(lo,hi)) -> (max lo (((total-upper) `quot` w)+hi),
                          min hi (((total-lower) `quot` w)+lo))) $ zip weights constraints

weightedSum :: [Int] -> [Int] -> Int
weightedSum weights values = sum $ map (\(w,x) -> w*x) (zip weights values)

-- a + b + c = 10 ; a:[1..5], b:[3..7], c[2..4]
-- lowerLimit = 6
-- upperLimit = 16
-- lowerLimits = [10-(16-5),10-(16-7),10-(16-4)] = [-1,1,-2]
-- upperLimits = [10-(6-1),10-(6-3),10-(6-2)] = [5,7,6]
