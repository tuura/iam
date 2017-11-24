module Subtractor.Examples.Swap where

import Prelude hiding (subtract)
import Text.Pretty.Simple (pPrint)
import Data.SBV hiding (label)
import Subtractor
import Subtractor.Types
import Subtractor.State
import Subtractor.Assembly
import Subtractor.Semantics
import Subtractor.Examples.Common

swap :: Script
swap = do
    load r0 0
    load r1 1
    store r1 0
    store r0 1
    halt

theoremSwap :: Symbolic SBool
theoremSwap = do
    x <- forall "x"
    y <- forall "y"
    let initialState = templateState swap (initialiseMemory [(0, x), (1, y)])
        finalState = verify steps $ initialState
    let x' = readArray (memory finalState) 0
    let y' = readArray (memory finalState) 1
    pure $ (x' .== y) &&& (y' .== x)

provingSwap :: IO ()
provingSwap = do
    result <- proveWith prover theoremSwap
    print result

simulatingSwap :: IO ()
simulatingSwap = do
    let finalState = verify steps $ templateState swap (initialiseMemory [(0, 42)])
        memoryDump = dumpMemory 0 5 $ memory finalState
    putStr "Memory Dump: "
    print memoryDump
    putStr "Flags register Dump: "
    print $ "Halted: " ++ show (readArray (flags finalState) (flagId Halted))
    putStr "Final state: "
    pPrint finalState
