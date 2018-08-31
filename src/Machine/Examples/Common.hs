module Machine.Examples.Common where

import Text.Pretty.Simple
import qualified Data.Map.Strict as Map
import Machine.Types
import Machine.Instruction
import Machine.Instruction.Encode
import Machine.Instruction.Decode
import Machine.Program
import Machine.Semantics.Simulate

emptyRegisters :: RegisterBank
emptyRegisters = Map.fromList $ zip [R0, R1, R2, R3] [0, 0..]

emptyFlags :: Flags
emptyFlags = Map.fromList $ zip [Zero, Overflow, Halted] [0, 0..]

initialiseMemory :: [(MemoryAddress, Value)] -> Memory
initialiseMemory m =
    let blankMemory = Map.fromList $ zip [0..1023] [0, 0..]
    in foldr (\(addr, value) acc -> Map.adjust (const value) addr acc) blankMemory m

dumpMemory :: Value -> Value -> Memory -> [Value]
dumpMemory from to m = map ((Map.!) m) [from..to]

boot :: Program -> Memory -> MachineState
boot prog mem = MachineState { registers = emptyRegisters
                            , instructionCounter = 0
                            , instructionRegister = 0 -- encode $ Jump 0
                            , program = prog
                            , flags = emptyFlags
                            , memory = mem
                            , clock = 0
                            }

--------------------------------------------------------------------------------
addExample :: Int -> Value -> Value -> IO ()
addExample steps x y = do
    prog <- readProgram "examples/add.asm"
    let mem = initialiseMemory [(0, x), (1, y)]
        initialState = boot prog mem
        finalState = runModel steps initialState
    print . registers $ finalState
    print . decode . instructionRegister $ finalState
    print . instructionCounter $ finalState
    print . flags $ finalState
    -- pPrint $ finalState
--------------------------------------------------------------------------------
gcdExample :: Int -> Value -> Value -> IO ()
gcdExample steps x y = do
    prog <- readProgram "examples/gcd.asm"
    let mem = initialiseMemory [(0, x), (1, y)]
        initialState = boot prog mem
        finalState = runModel steps initialState
    print . registers $ finalState
    print . registers $ finalState
    print . decode . instructionRegister $ finalState
    print . instructionCounter $ finalState
    print . flags $ finalState