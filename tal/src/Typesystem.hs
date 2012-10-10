-----------------------------------------------------------------------------
--
-- Module      :  Typesystem
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

module Typesystem (

    RegisterTypes,
    HeapTypes,
    Type(..),
    typeCheck,
    verifyInstructionSequence,
    verifyInstructions,
    verifyHeap
) where

import Tal

type RegisterTypes = [Type]

type HeapTypes = [(String,Type)]

data Type = IntType |
    Code RegisterTypes |
    TypeVariable Int |
    UniversalType (Type -> Type)

lexicallyEqual :: Type -> Type -> Bool
lexicallyEqual IntType IntType = True
lexicallyEqual (TypeVariable n1) (TypeVariable n2) = n1 == n2
lexciallyEqual (Code rft1) (Code rft2) = and (map (\t -> lexicallyEqual (fst t) (snd t)) $ zip rft1 rft2)
lexciallyEqual _ _ = False

match :: Type -> Type -> [(Int, Type)] -> Maybe [(Int, Type)]
match IntType IntType b = Just b
match (Code rft1) (Code rft2) b = foldl (\b' (t1,t2) -> case b' of
    Nothing -> Nothing
    Just b'' -> match t1 t2 b'') (Just b) $ zip rft1 rft2
match (TypeVariable v) t b = case lookup v b of
    Nothing -> Just ((v, t) : b)
    Just t' -> if lexicallyEqual t' t then Just b else Nothing
match _ _ _ = Nothing

matches :: Type -> Type -> Bool
matches t1 t2 = case match (instantiate t1) (instantiate t2) [] of
    Nothing -> False
    Just _ -> True

-- replaces all universal types with type variables
instantiate :: Type -> Type
instantiate t = fst $ instantiate' t 1

instantiate' :: Type -> Int -> (Type, Int)
instantiate' (Code rft) n = let (t,n') = foldr instantiate'' ([], n) rft in (Code t,n)
instantiate' (UniversalType tf) n = instantiate' (tf (TypeVariable n)) (n+1)
instantiate' t n = (t, n)

instantiate'' :: Type -> ([Type],Int) -> ([Type],Int)
instantiate'' t (ts,n) = let (t',n') = instantiate' t n in (t':ts,n')


typeCheck :: MachineState -> RegisterTypes -> HeapTypes -> [String]
typeCheck (h,r,i) rfts hts = (verifyRegisters r rfts hts) ++ (verifyHeap h hts)

verifyRegisters :: Registers -> RegisterTypes -> HeapTypes -> [String]
verifyRegisters rf rft ht = foldl (++) [] (map (verifyOperand ht) (zip rf rft))

verifyOperand :: HeapTypes -> (Operand,Type) -> [String]
verifyOperand ht (op,t) = if matches (operandType [] ht op) t then [] else ["Expected " ++ show op ++ " to have type " ++ show t]

operandType :: RegisterTypes -> HeapTypes -> Operand -> Type
operandType _ _ (Literal _) = IntType
operandType _ ht (Label s) = case lookup s ht of
    Just st -> st
operandType rft _ (Register i) = get i rft

get :: Int -> [a] -> a
get 1 (a:_) = a
get i (_:as) = get (i-1) as

set :: Int -> a -> [a] -> [a]
set 1 o (_:os) = o:os
set i o (o1:os) = o1 : set (i-1) o os

verifyHeap :: Heap -> HeapTypes -> [String]
verifyHeap h ht = concat $ map (verifyInstructions ht) h

verifyInstructions :: HeapTypes -> (String, HeapValue) -> [String]
verifyInstructions ht (s, Instructions i) = let
  Just (Code r) = lookup s ht
  in verifyInstructionSequence r ht i

verifyInstructionSequence :: RegisterTypes -> HeapTypes -> InstructionSequence -> [String]
verifyInstructionSequence rft ht (Jump op) = case operandType rft ht op of
    Code r -> if matches (Code r) (Code rft) then [] else ["Can't execute jump"]
    UniversalType f -> if matches (UniversalType f) (Code rft) then [] else ["Can't execute jump"]
    otherwise -> ["Operand " ++ show op ++ " does not refer to a label"]
verifyInstructionSequence rft ht (Sequence i is) = let
    (s',rft') = instructionType rft ht i
    s'' = verifyInstructionSequence rft' ht is
    in s' ++ s''

instructionType :: RegisterTypes -> HeapTypes -> Instruction -> ([String], RegisterTypes)
instructionType rft ht (Assign i op) = let opType = operandType rft ht op in ([], set i opType rft)
instructionType rft ht (Add d s op) = case operandType rft ht op of
    IntType -> case operandType rft ht (Register s) of
                    IntType -> ([], set d IntType rft)
                    t -> (["Expected number but got " ++ show t], set d IntType rft)
    t -> (["Expected number but got " ++ show t], set d IntType rft)
instructionType rft ht (If i op) = case operandType rft ht (Register i) of
    IntType -> case operandType rft ht op of
        Code r -> ([], rft)
        t -> (["Expected label but got " ++ show t], rft)
    t -> (["Expected number but got " ++ show t], rft)

instance Show Type where
    show t = showType t 1

showType :: Type -> Int -> String
showType IntType _ = "::Int"
showType (Code rft) n = foldl (\str s -> str ++ s ++ ";") "{" $ map (\(n,t) -> "r" ++ show n ++ ":" ++ showType t n)  (zip [1..] rft)
showType (TypeVariable n) _ = "var" ++ show n
showType (UniversalType f) n = "var" ++ show n ++ "." ++ showType (f (TypeVariable n)) (n+1)
