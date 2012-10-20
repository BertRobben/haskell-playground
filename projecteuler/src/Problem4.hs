-----------------------------------------------------------------------------
--
-- Module      :  Problem4
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

module Problem4 (
  largestPalindrome
) where

largestPalindrome :: Int -> Int
largestPalindrome n = maximum $ map (lp $ numbers n) (numbers n)

lp :: [Int] -> Int -> Int
lp lst n = case filter isPalindrome (map (*n) lst) of
    [] -> 0
    l -> maximum l

numbers :: Int -> [Int]
numbers n = [10^(n-1)+1..10^n-1]

isPalindrome :: Int -> Bool
isPalindrome n = (rev n) == n

rev :: Int -> Int
rev n = reverse' n 0

reverse' :: Int -> Int -> Int
reverse' n result = if n == 0
    then result
    else reverse' (div n 10) ((result * 10) + (mod n 10))


