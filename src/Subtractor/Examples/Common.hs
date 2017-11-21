module Subtractor.Examples.Common where

import Data.SBV
import Subtractor.Types
import Subtractor.State
import Subtractor.Assembly

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
                               , instructionRegister = 0 -- Jmpi 0
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
steps = 15