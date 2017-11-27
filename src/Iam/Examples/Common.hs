module Iam.Examples.Common where

import Data.SBV
import Iam.Types
import Iam.State
import Iam.Assembly

r0, r1, r2, r3 :: Register
[r0, r1, r2, r3] = [0..3]

emptyRegisters :: RegisterBank
emptyRegisters = mkSFunArray $ const 0

emptyFlags :: Flags
emptyFlags = mkSFunArray $ const false

initialiseMemory :: [(MemoryAddress, Value)] -> Memory
initialiseMemory =
    foldr (\(a, v) m -> writeArray m a v) (mkSFunArray $ const 0)

dumpMemory :: Word8 -> Word8 -> Memory -> [SWord64]
dumpMemory from to m = map (readArray m) [literal from..literal to]

templateState :: Script -> Memory -> MachineState
templateState src mem = MachineState { registers = emptyRegisters
                               , instructionCounter = 0
                               , instructionRegister = Jump 0
                               , program = assemble src
                               , flags = emptyFlags
                               , memory = mem
                               , clock = 0
                               }

prover = z3 { verbose = True
            , redirectVerbose = Just "example.smt2"
            , timing = PrintTiming
            , printBase = 10
            }

steps :: Int
steps = 100