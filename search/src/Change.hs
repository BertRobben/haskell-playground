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

module Change (

) where

without :: (Eq a) => a -> [a] -> [a]
without _ [] = []
without x (y:xs) = if (x == y) then xs else y : (without x xs)

return_change :: Integer -> [Integer] -> [[Integer]]
return_change amount coins = concat $ map (return_change' amount coins) coins

return_change' :: Integer -> [Integer] -> Integer -> [[Integer]]
return_change' amount coins coin = if (amount < coin)
    then []
    else (if amount == coin
        then [[coin]]
        else map ((:) coin) (return_change (amount - coin) (without coin coins)))

change :: Integer -> [Integer] -> [[Integer]]
change amount [] = []
change amount (coin:coins) = (change amount coins) ++ (case compare (amount-coin) 0 of
    GT -> map ((:) coin) (change (amount - coin) coins)
    EQ -> [[]]
    LT -> []
    )

change' :: Integer -> [Integer] -> Integer
change' 0 _ = 1
change' _ [] = 0
change' amount c@(coin:coins) = (change' amount coins) + (if amount >= coin then
    change' (amount - coin) c else 0)

return_change'' :: Integer -> [Integer] -> Integer -> [[Integer]]
return_change'' amount coins coin = if (amount < coin)
    then []
    else (if amount == coin
        then [[coin]]
        else map ((:) coin) (return_change (amount - coin) (without coin coins)))
