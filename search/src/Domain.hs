-----------------------------------------------------------------------------
--
-- Module      :  Domain
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
module Domain (

  SearchSpace(..),
  Constraint(..),
  VariableMap,
  Domain,
  solve

) where

import qualified Data.Map as Map

type Domain = [Int]
type Variable = (String, Domain)
type VariableMap = Map.Map String Domain

lookupVar :: String -> VariableMap -> Maybe Variable
lookupVar s m = fmap (\d -> (s, d)) (Map.lookup s m)

data SearchSpace = SearchSpace {
    vars :: VariableMap,
    constraints :: [(Constraint, [String])]
    }

data Constraint = Constraint {
    valid :: [Domain] -> Bool,
    propagate :: [Domain] -> [Domain]
    }

instance Eq SearchSpace where
  s1 == s2 = vars s1 == vars s2

type Solution = [(String, Int)]

empty :: SearchSpace -> Bool
empty s = or (map null (Map.elems $ vars s))

consistent :: SearchSpace -> Bool
consistent s = and $ map (valid' (vars s)) (constraints s)

valid' :: VariableMap -> (Constraint, [String]) -> Bool
valid' m (c,names) = valid c (map (\s -> lookupDomain s m) names)

lookupDomain :: String -> VariableMap -> Domain
lookupDomain s m = case lookupVar s m of
    Nothing -> []
    Just v -> snd v

solve :: SearchSpace -> [Solution]
solve s = let s' = fixPoint propagateConstraints s in
    case asSolution s' of
        Nothing -> map xxx (guesses s')
        Just sol -> [sol]

-- fixPoint propagateConstraints

asSolution :: SearchSpace -> Maybe Solution
asSolution s = asSolution' $ Map.toList (vars s)

asSolution' :: [(String,[Int])] -> Maybe Solution
asSolution' [] = Just []
asSolution' ((s,[x]):lst) = fmap ((:) (s,x)) (asSolution' lst)
asSolution' _ = Nothing

guesses :: SearchSpace -> [(String,Int)]
guesses s = concat $ map expand (Map.toList (vars s))

expand :: (String,[Int]) -> [(String,Int)]
expand (_,[]) = []
expand (_,[_]) = []
expand (name,lst) = map (\d -> (name,d)) lst

guess :: (String,[Int]) -> Maybe (String,Int)
guess s = concat $ map expand (Map.toList (vars s))

first :: [a] -> Maybe a
first [] = Nothing
first (x:_) = x

propagateConstraints :: SearchSpace -> SearchSpace
propagateConstraints s = foldr propagateConstraint s (constraints s)

propagateConstraint :: (Constraint, [String]) -> SearchSpace -> SearchSpace
propagateConstraint (c, names) s = let d = propagate c (map (\name -> lookupDomain name (vars s)) names) in
    SearchSpace (updateVars (zip names d) (vars s)) (constraints s)

updateVar :: (Ord k) => k -> v -> Map.Map k [v] -> Map.Map k [v]
updateVar k v m = Map.insert k [v] m

updateVars :: (Ord k) => [(k,a)] -> Map.Map k a -> Map.Map k a
updateVars v m = foldr (\(k,v) m -> Map.insert k v m) m v

fixPoint :: (Eq a) => (a -> a) -> a -> a
fixPoint f a = let fa = f a in
    if fa == a then a else fixPoint f fa
