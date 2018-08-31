module Machine.Examples.Common where

import Text.Pretty.Simple
import qualified Data.Map.Strict as Map
import Machine.Types
import Machine.Instruction
import Machine.Instruction.Encode
import Machine.Instruction.Decode
import Machine.Program
import Machine.Semantics.Simulate

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