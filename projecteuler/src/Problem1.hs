-----------------------------------------------------------------------------
--
-- Module      :  Problem1
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

module Problem1 (
 solve

) where

solve :: Int -> Int
solve n = sum (filter (\n -> (mod n 3 == 0) || (mod n 5 == 0)) [1..(n-1)])


