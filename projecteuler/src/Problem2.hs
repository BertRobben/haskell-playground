-----------------------------------------------------------------------------
--
-- Module      :  Problem2
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

module Problem2 (
  solve
) where

solve :: Int -> Int
solve max = solve' max (2, 1, 2)

solve' :: Int -> (Int, Int, Int) -> Int
solve' max t@(m, n, s) = if (m > max) then s else solve' max (next t)

fib :: Int -> Int
fib 1 = 1
fib 2 = 2
fib n = (fib (n-1)) + (fib (n-2))

next :: (Int, Int, Int) -> (Int, Int, Int)
next (m, n, s) = (m+n,m,if even (m+n) then s+m+n else s)
