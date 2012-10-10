-----------------------------------------------------------------------------
--
-- Module      :  Tal
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

module Tal (

  Operand(..),
  Instruction(..),
  InstructionSequence(..),
  Registers,
  Heap,
  HeapValue(..),
  MachineState,
  eval,
  rewrite

) where

data Operand = Literal Int | Label String | Register Int | Uptr HeapValue

data Instruction = Assign Int Operand |
  Add Int Int Operand |
  If Int Operand |
  Load Int Int Int |
  Store Int Int Int |
  Malloc Int Int |
  Commit Int |
  Salloc Int |
  Sfree Int

data HeapValue = Instructions InstructionSequence | Data [Operand] deriving Show

data InstructionSequence = Jump Operand |
 Sequence Instruction InstructionSequence

type Registers = [Operand]

type Heap = [(String, HeapValue)]

type MachineState = (Heap, Registers, InstructionSequence)

eval :: MachineState -> Int -> MachineState
eval m 0 = m
eval m i = case rewrite m of
    Nothing -> m
    Just m' -> eval m' (i-1)
--eval m = case rewrite m of
--    Nothing -> m
--    Just m' -> eval m'

rewrite :: MachineState -> Maybe MachineState
rewrite (h, r, Jump op) = deref (r' r op) h >>= (\is -> Just (h, r, is))
rewrite (h, r, (Sequence (Assign i op) is)) = case r' r op of
    Uptr _ -> Nothing
    v -> Just (h, set i v r, is)
rewrite (h, r, (Sequence (Add regd regs i) is)) = do
    v1 <- valueOf (get regs r)
    v2 <- valueOf (r' r i)
    Just (h, set regd (Literal (v1+v2)) r, is)
rewrite (h, r, (Sequence (If i op) is)) = do
    z <- valueOf (get i r)
    jumpLocation <- deref op h
    Just (h, r, if (z==0) then jumpLocation else is)
rewrite (h, r, (Sequence (Malloc d n) is)) = Just (h, set d (malloc n) r, is)
rewrite (h, r, (Sequence (Commit i) is)) = do
    t <- uptrOf (get i r)
    l <- Just $ newLabel h
    Just ((l,Data t) : h, set i (Label l) r, is)
rewrite (h, r, (Sequence (Load d s op) is)) = do
    v <- uptrOf (r' r op)
    v -> Just (h, set i v r, is)

malloc :: Int -> Operand
malloc n = Uptr $ Data (map Literal (take n [1..]))

newLabel :: Heap -> String
newLabel h = "l" ++ show (length h)

r' :: Registers -> Operand -> Operand
r' r (Register i) = get i r
r' _ o = o

get :: Int -> [Operand] -> Operand
get _ [] = Literal 0
get 1 (o:_) = o
get i (_:os) = get (i-1) os

set :: Int -> Operand -> [Operand] -> [Operand]
set 1 o [] = [o]
set i o [] = (Literal 0) : set (i-1) o []
set 1 o (_:os) = o:os
set i o (o1:os) = o1 : set (i-1) o os

deref :: Operand -> Heap -> Maybe InstructionSequence
deref (Label l) h = case lookup l h of
    Just(Instructions c) -> Just c
    otherwise -> Nothing
deref _ _ = Nothing

valueOf :: Operand -> Maybe Int
valueOf (Literal i) = Just i
valueOf _= Nothing

uptrOf :: Operand -> Maybe [Operand]
uptrOf (Uptr (Data v)) = Just v
uptrOf _= Nothing

instance Show Operand where
    show (Literal i) = show i
    show (Label l) = "'" ++ l ++ "'"
    show (Register r) = "r" ++ show r

instance Show InstructionSequence where
    show (Jump l) = "jump " ++ (show l)
    show (Sequence i is) = (show i) ++ "; " ++ (show is)

instance Show Instruction where
    show (Assign i op) = "r" ++ (show i) ++ "=" ++ (show op)
    show (Add s d op) = "r" ++ (show s) ++ "=r" ++ (show d) ++ "+" ++ (show op)
    show (If i l) = "if r" ++ (show i) ++ " jump " ++ (show l)

