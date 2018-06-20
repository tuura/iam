module Machine.Examples.Common where

import Data.SBV
import Machine.Semantics.Symbolic
import Machine.Semantics.Symbolic.Types
import Machine.Semantics.Symbolic.Instruction
import Machine.Semantics.Symbolic.State
import Machine.Semantics.Symbolic.Machine
import Machine.Assembly

emptyRegisters :: RegisterBank
emptyRegisters = mkSFunArray $ const 0

emptyFlags :: Flags
emptyFlags = mkSFunArray $ const 0

initialiseMemory :: [(SBV MemoryAddress, SBV Value)] -> Memory
initialiseMemory =
    foldr (\(a, v) m -> writeArray m a v) (mkSFunArray $ const 0)

dumpMemory :: Value -> Value -> Memory -> [SBV Value]
dumpMemory from to m = map (readArray m) [literal from..literal to]

templateState :: Script -> Memory -> MachineState
templateState src mem = MachineState { registers = emptyRegisters
                               , instructionCounter = 0
                               , instructionRegister = literal $ Jump 0
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
