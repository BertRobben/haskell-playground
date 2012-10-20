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
allDifferent = propagateAllDifferent

propagateAllDifferent :: (Eq a) => [[a]] -> [[a]]
propagateAllDifferent v = propagateAllDifferent' (length v) v

propagateAllDifferent' :: (Eq a) => Int -> [[a]] -> [[a]]
propagateAllDifferent' 0 v = v
propagateAllDifferent' i v = case head (drop (i-1) v) of
    [j] -> propagateAllDifferent' (i-1) (evict (i,j) v)
    _ -> propagateAllDifferent' (i-1) v

evict :: (Eq a) => (Int, a) -> [[a]] -> [[a]]
evict _ [] = []
evict (1,i) (l:ls) = l : evict (0,i) ls
evict (n,i) (l:ls) = (without i l) : evict (n-1,i) ls
