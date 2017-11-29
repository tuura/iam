module Machine where

import Text.Pretty.Simple (pPrint)
import Data.SBV
import Machine.Types

import Machine.State
import Machine.Semantics

verify :: Int -> MachineState -> MachineState
verify steps state
    | steps == 0 = state
    | otherwise  = ite halted state (verify (steps - 1) nextState)
  where
    halted    = readArray (flags state) (flagId Halted)
    nextState = snd $ run executeInstruction state
