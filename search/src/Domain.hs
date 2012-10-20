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
  solve,
  without

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

type Constraint = [Domain] -> [Domain]

instance Eq SearchSpace where
  s1 == s2 = vars s1 == vars s2

type Solution = [(String, Int)]

empty :: SearchSpace -> Bool
empty s = or (map null (Map.elems $ vars s))

lookupDomain :: String -> VariableMap -> Domain
lookupDomain s m = case lookupVar s m of
    Nothing -> []
    Just v -> snd v

solve :: SearchSpace -> [Solution]
solve s = let s' = fixPoint propagateConstraints s in
    if empty s' then [] else
        case asSolution s' of
            Nothing -> concat (map solve (makeGuess s))
            Just sol -> [sol]

-- fixPoint propagateConstraints

asSolution :: SearchSpace -> Maybe Solution
asSolution s = asSolution' $ Map.toList (vars s)

asSolution' :: [(String,[Int])] -> Maybe Solution
asSolution' [] = Just []
asSolution' ((s,[x]):lst) = fmap ((:) (s,x)) (asSolution' lst)
asSolution' _ = Nothing

makeGuess :: SearchSpace -> [SearchSpace]
makeGuess s = let (name, value) = guess s in
    [withValue s name value, withoutValue s name value]

withValue :: SearchSpace -> String -> Int -> SearchSpace
withValue s name value = SearchSpace (updateVar name value (vars s)) (constraints s)

withoutValue :: SearchSpace -> String -> Int -> SearchSpace
withoutValue s name value = SearchSpace
    (Map.insert name (without value (lookupDomain name (vars s))) (vars s))
    (constraints s)

without :: (Eq a) => a -> [a] -> [a]
without _ [] = []
without x (y:xs) = if (x == y) then xs else y : (without x xs)

guess :: SearchSpace -> (String,Int)
guess s = guess' (Map.toList (vars s))

guess' :: [(String,[a])] -> (String, a)
guess' ((name,v1:v2:_):_) = (name, v1)
guess' (_:next) = guess' next

propagateConstraints :: SearchSpace -> SearchSpace
propagateConstraints s = foldr propagateConstraint s (constraints s)

propagateConstraint :: (Constraint, [String]) -> SearchSpace -> SearchSpace
propagateConstraint (c, names) s = let d = c (map (\name -> lookupDomain name (vars s)) names) in
    SearchSpace (updateVars (zip names d) (vars s)) (constraints s)

updateVar :: (Ord k) => k -> v -> Map.Map k [v] -> Map.Map k [v]
updateVar k v m = Map.insert k [v] m

updateVars :: (Ord k) => [(k,a)] -> Map.Map k a -> Map.Map k a
updateVars v m = foldr (\(k,v) m -> Map.insert k v m) m v

fixPoint :: (Eq a) => (a -> a) -> a -> a
fixPoint f a = let fa = f a in
    if fa == a then a else fixPoint f fa
