{-# LANGUAGE RankNTypes #-}

module Machine.Examples.Common where

import Control.Selective
-- import Text.Pretty.Simple
import qualified Data.Map.Strict as Map
import Machine.Types
import Machine.Instruction
import Machine.Program
import Machine
import Machine.Simulator
import FS
import Machine.Graph

--------------------------------------------------------------------------------
addProgram :: Program
addProgram = [  (0, Instruction $ Load R0 0)
             ,  (1, Instruction $ Add R0 1)
             ,  (2, Instruction $ Halt)
             ]

addExample :: Int -> MachineValue -> MachineValue -> IO ()
addExample steps x y = do
    -- prog <- readProgram "examples/add.asm"
    let mem = initialiseMemory [(0, x), (1, y)]
        initialState = boot addProgram mem
        finalState = runModel steps initialState
    print . registers $ finalState
    print . instructionRegister $ finalState
    print . instructionCounter $ finalState
    print . flags $ finalState
    -- pPrint $ finalState

addDeps = FS.dependencies (semantics' (map snd $ addProgram))
--------------------------------------------------------------------------------
gcdProgram :: Program
gcdProgram = zip [0..]
    -- # Find the greatest common divisor of values in memory locations 0 and 1,
    -- # put result to the register R1
    [ Instruction (Set R0 0)
    , Instruction (Store R0 255)
    , Instruction (Load R0 1)
    -- # Test register R0 for being zero by subtracting zero
    , Instruction (Sub R0 255)
    -- # Halt if register R0 contains zero, loop otherwise
    , Instruction (JumpZero 6)
    , Instruction (Load R0 0)
    , Instruction (Mod R0 1)
    , Instruction (Load R1 1)
    , Instruction (Store R0 1)
    , Instruction (Store R1 0)
    , Instruction (Jump (-8))
    , Instruction Halt
    ]

gcdExample :: Int -> MachineValue -> MachineValue -> IO ()
gcdExample steps x y = do
    -- prog <- readProgram "examples/gcd.asm"
    let prog = gcdProgram
    let mem = initialiseMemory [(0, x), (1, y)]
        initialState = boot prog mem
        finalState = runModel steps initialState
    print . registers $ finalState
    print . registers $ finalState
    print . instructionRegister $ finalState
    print . instructionCounter $ finalState
    print . clock $ finalState
    print . flags $ finalState

gcdDeps = FS.dependencies (semantics' (map snd $ gcdProgram))