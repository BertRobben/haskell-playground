-----------------------------------------------------------------------------
--
-- Module      :  AllDifferent
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

module AllDifferent (

allDifferent

) where

import Domain

allDifferent :: Constraint
allDifferent = Constraint validateAllDifferent propagateAllDifferent

validateAllDifferent :: [[Int]] -> Bool
validateAllDifferent v = unique $ lockedVariables v

propagateAllDifferent :: [[Int]] -> [[Int]]
propagateAllDifferent v = propagateAllDifferent' (length v) v

propagateAllDifferent' :: Int -> [[Int]] -> [[Int]]
propagateAllDifferent' 0 v = v
propagateAllDifferent' i v = case head (drop (i-1) v) of
    [j] -> propagateAllDifferent' (i-1) (evict (i,j) v)
    _ -> propagateAllDifferent' (i-1) v

apply :: [[Int]] -> (Int,[Int]) -> [[Int]]
apply v (i,[j]) = evict (i,j) v
apply v _ = v

evict :: (Int, Int) -> [[Int]] -> [[Int]]
evict _ [] = []
evict (1,i) (l:ls) = l : evict (0,i) ls
evict (n,i) (l:ls) = (without i l) : evict (n-1,i) ls

unique :: (Eq a) => [a] -> Bool
unique [] = True
unique (x:xs) = not (elem x xs) && unique xs

lockedVariables :: [[Int]] -> [Int]
lockedVariables vars = concat (filter singleValued vars)

singleValued :: [a] -> Bool
singleValued ([_]) = True
singleValued _ = False

without :: (Eq a) => a -> [a] -> [a]
without _ [] = []
without x (y:xs) = if (x == y) then xs else y : (without x xs)
