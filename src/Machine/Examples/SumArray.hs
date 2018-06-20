{-# LANGUAGE ConstraintKinds, RankNTypes,
             ScopedTypeVariables,
             FlexibleContexts,
             FlexibleInstances #-}
module Machine.Examples.SumArray where

import Data.Maybe (fromJust)
import Prelude hiding (Monad, subtract)
import qualified Prelude (Monad)
import Text.Pretty.Simple (pPrint)
import Data.SBV hiding (label)
import Control.Selective
import Metalanguage
import Machine.Semantics (blockSemanticsM, MachineKey)
import Machine.Semantics.Symbolic
import Machine.Semantics.Symbolic.Machine
import Machine.Semantics.Symbolic.Types
import Machine.Semantics.Symbolic.Instruction
import Machine.Semantics.Symbolic.State
import Machine.Assembly
-- import Machine.Semantics (readRegister)
import Machine.Examples.Common

-- prover = z3 { verbose = True
--             , redirectVerbose = Just "example.smt2"
--             , timing = PrintTiming
--             , printBase = 10
--             }

-- initialiseMemory :: [(SBV MemoryAddress, SBV Value)] -> Memory
-- initialiseMemory =
--     foldr (\(a, v) m -> writeArray m a v) (mkSFunArray $ const 0)

type Monad m = (Selective m, Prelude.Monad m)

runModel :: Int -> MachineState -> MachineState
runModel steps state
    | steps == 0 = state
    | otherwise  = ite halted state (runModel (steps - 1) nextState)
  where
    halted    = (./= 0) $ readArray (flags state) (literal Halted)
    nextState = snd $ run executeInstruction state

runDaFuckingScript :: Script -> MachineState -> MachineState
runDaFuckingScript src state =
    -- let semantics = (blockSemanticsM . instructions $ src) :: Semantics Monad MachineKey Value ()
    -- in snd $ run (interpretSymbolic . fromJust $ buildAST semantics) state
    snd $ run (interpretSymbolic . fromJust $ buildAST (blockSemanticsM . instructions $ src)) state

--------------------------------------------------------------------------------
sumArray :: Script
sumArray = do
    load R0 0       -- sum := 0, sum accumulator
    load R2 254     -- i := n,   loop counter
    loop <- label
    store R2 255
    loadMI R1 255   -- load a[i]
    store R1 254    -- put a[i] to cell 254
    add R0 254      -- sum := sum + a[i]
    add R2 253      -- i := i - 1
    jumpZero 1
    goto loop
    halt

theoremSumArray :: Int -> IO ThmResult
theoremSumArray n = proveWith prover $ do
    summands <- mkForallVars n
    let memory = initialiseMemory $  [(0, 0)]
                                  ++ zip [1..] summands
                                  ++ [ (253, -1)
                                     , (254, fromIntegral n)
                                     ]
        steps = 10000
        finalState = runModel steps $ templateState (assemble sumArray) memory
        result = readArray (registers finalState) (literal R0)
    pure $ result .== sum summands &&& clock finalState .< 10000

daFuckingTheorem :: Int -> IO ThmResult
daFuckingTheorem n = proveWith prover $ do
    summands <- mkForallVars n
    let memory = initialiseMemory $  [(0, 0)]
                                  ++ zip [1..] summands
                                  ++ [ (253, -1)
                                     , (254, fromIntegral n)
                                     ]
        steps = 10000
        finalState = runDaFuckingScript sumArray (templateState (assemble sumArray) memory)
        result = readArray (registers finalState) (literal R0)
    pure $ result .== sum summands &&& clock finalState .< 10000