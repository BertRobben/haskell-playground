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

import Tal
import Typesystem

start = Sequence (Assign 1 (Literal 2)) $ Sequence (Assign 2 (Literal 3)) $ Sequence (Assign 4 (Label "end")) $ Jump (Label "prod")

prod = Instructions $ Sequence (Assign 3 (Literal 0)) $ Jump (Label "loop")

end = Instructions $ Jump (Label "next")

loop = Instructions $ Sequence
  (If 1 (Label "done"))
  $ Sequence (Add 3 2 (Register 3))
  $ Sequence (Add 1 1 (Literal (-1)))
  $ Jump (Label "loop")

done = Instructions $ Jump (Register 4)

tau = Code [IntType, IntType, IntType, UniversalType (\t -> Code [IntType, IntType, IntType, t])]

heap = [("loop", loop), ("done", done), ("prod", prod)]
heapTypes = [("loop", tau), ("done", tau), ("prod", tau)]

main = do
    putStr $ concat (verifyInstructions heapTypes ("done", done))
--    putStr $ concat (verifyHeap heap heapTypes)
    Main.log ([("loop", loop), ("done", done), ("prod", prod), ("return", end)], [], start)

--log Nothing = return
--log (Just m) = do
--   putStrLn $ show m
--    return Main.log $ rewrite m


log m = do
    putStrLn $ show m
    case rewrite m of
        Just m' -> Main.log m'
        Nothing -> return ()
