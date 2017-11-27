module Iam where

import Text.Pretty.Simple (pPrint)
import Data.SBV
import Iam.Types

import Iam.State
import Iam.Semantics

verify :: Int -> MachineState -> MachineState
verify steps state
    | steps == 0 = state
    | otherwise  = ite halted state (verify (steps - 1) nextState)
  where
    halted    = readArray (flags state) (flagId Halted)
    nextState = snd $ run executeInstruction state
