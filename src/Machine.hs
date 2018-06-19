module Machine where

import Text.Pretty.Simple (pPrint)
import Data.SBV
import Machine.Types

import Machine.State
import Machine.Semantics

runModel :: Int -> MachineState -> MachineState
runModel steps state
    | steps == 0 = state
    | otherwise  = ite halted state (runModel (steps - 1) nextState)
  where
    halted    = readArray (flags state) (literal Halted)
    nextState = snd $ run executeInstruction state
