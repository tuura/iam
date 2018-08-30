module Machine.Examples.Common where

import qualified Data.Map.Strict as Map
import Machine.Types
import Machine.Instruction
import Machine.Instruction.Encode
import Machine.Program
import Machine.Semantics.Simulate

emptyRegisters :: RegisterBank
emptyRegisters = Map.fromList $ zip [R0, R1, R2, R3] [0..]

emptyFlags :: Flags
emptyFlags = Map.fromList $ zip [Zero, Overflow, Halted] [0..]

initialiseMemory :: [(MemoryAddress, Value)] -> Memory
initialiseMemory m =
    let blankMemory = Map.fromList $ zip [0..255] [0..]
    in foldr (\(addr, value) acc -> Map.adjust (const value) addr acc) blankMemory m

dumpMemory :: Value -> Value -> Memory -> [Value]
dumpMemory from to m = map ((Map.!) m) [from..to]

boot :: Program -> Memory -> MachineState
boot prog mem = MachineState { registers = emptyRegisters
                            , instructionCounter = 0
                            , instructionRegister = encode $ Jump 0
                            , program = prog
                            , flags = emptyFlags
                            , memory = mem
                            , clock = 0
                            }

steps :: Int
steps = 100

