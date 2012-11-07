{-# LANGUAGE CPP, TemplateHaskell #-}
-----------------------------------------------------------------------------
--
-- Module      :  Main
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

module Main (
    main
) where

import SearchSpace
import Domain
import Constraint.AllDifferent
import Abc
import Sudoku
import Anagram
import Constraint.Sum
import qualified Data.Map as Map

data SampleSpace = SampleSpace { one::[Int], two::[Int], three::[Int] } deriving Show

x = Map.fromList [("a", [])]

sampleSpace :: Int -> SearchSpace
sampleSpace n = SearchSpace
    (Map.fromList [("one", [1..n]), ("two", [1..n]), ("three", [1..n])])
    [(allDifferent, ["one", "two", "three"])]

-- Hello World
main = do
    putStrLn (show $ solve (sampleSpace 3))
