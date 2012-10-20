-----------------------------------------------------------------------------
--
-- Module      :  Problem3
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

module Problem3 (

  largestPrimeFactor

) where

largestPrimeFactor :: Integer -> Integer
largestPrimeFactor n = let d = smallestDivisor n in
  if d == n then d else max d (largestPrimeFactor (div n d))

smallestDivisor :: Integer -> Integer
smallestDivisor n = smallestDivisor' n 2

smallestDivisor' n m = if mod n m == 0 then m else smallestDivisor' n (m+1)
