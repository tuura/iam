{-# LANGUAGE GeneralizedNewtypeDeriving #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Machine.Semantics.Symbolic.Machine
-- Copyright   :  (c) Georgy Lukyanov, Andrey Mokhov 2017
--
-- Maintainer  :  lukyanov.georgy@gmail.com
-- Stability   :  experimental
--
-- The simulation/verification framework.
--
-- This module contains all heavy-lifting: reading/writing memory, flags and
-- registers, controlling the clock and program execution.
--------------------------------------------------------------------------------
module Machine.Semantics.Symbolic.Machine where

import Data.Maybe (fromJust)
import Control.Monad.State.Strict
import Data.SBV
import Machine.Semantics.Symbolic.Types
import Machine.Semantics.Symbolic.State
import Machine.Semantics.Symbolic.Instruction

-- | Iam machine is a state transformer
newtype Machine a = Machine { runMachine :: State MachineState a }
    deriving (Functor, Applicative, Monad, MonadState MachineState)

run :: Machine a -> MachineState -> (a, MachineState)
run = runState . runMachine

--------------------------------------------------------------------------------
------------ Clock -------------------------------------------------------------
--------------------------------------------------------------------------------

-- | Advance the clock by a given number of clock cycles.
delay :: Clock -> Machine ()
delay cycles =
    modify $ \currentState ->
        currentState {clock = clock currentState + literal cycles}

--------------------------------------------------------------------------------
------------ Memory ------------------------------------------------------------
--------------------------------------------------------------------------------

-- | Write a new 'Value' to the given 'MemoryAddress'. We assume that it takes 2
-- clock cycles to access the memory in hardware.
writeMemory :: SBV MemoryAddress -> SBV Value -> Machine ()
writeMemory address value = do
    delay 2
    modify $ \currentState ->
        currentState {memory =
            writeArray (memory currentState) address value}

-- | Lookup the 'Value' at the given 'MemoryAddress'. If the value has never
-- been initialised, this function returns 0. We assume that it
-- takes 2 clock cycles to access the memory in hardware.
readMemory :: SBV MemoryAddress -> Machine (SBV Value)
readMemory address = do
    currentState <- get
    delay 2
    pure $ readArray (memory currentState) address

toMemoryAddress :: SBV Value -> Machine (SBV MemoryAddress)
toMemoryAddress value = do
    let valid = value .< 256
    return $ fromBitsLE (take 8 $ blastLE value)

--------------------------------------------------------------------------------
------------ Registers ---------------------------------------------------------
--------------------------------------------------------------------------------

-- | Lookup the 'Value' in a given 'Register'. If the register has never been
-- initialised, this function returns 0. We assume that it
-- takes 1 clock cycles to access a register in hardware.
readRegister :: SBV Register -> Machine (SBV Value)
readRegister register = do
    s <- get
    delay 1
    pure $ readArray (registers s) register

-- | Write a new 'Value' to a given 'Register'.
--   We assume that it takes 1 clock cycle to access a register in hardware.
writeRegister :: SBV Register -> SBV Value -> Machine ()
writeRegister register value = do
    delay 1
    modify $ \currentState ->
        currentState {registers = writeArray (registers currentState) register value}

--------------------------------------------------------------------------------
------------ Flags ---------------------------------------------------------
--------------------------------------------------------------------------------

-- | Lookup the value of a given 'Flag'. If the flag is not currently assigned
-- any value, it is assumed to be 'False'.
readFlag :: SBV Flag -> Machine (SBV Bool)
readFlag flag = do
    currentState <- get
    pure $ (./= 0) $ readArray (flags currentState) flag

-- | Set a given 'Flag' to the specified Boolean value.
--   We assume that it takes 1 clock cycle to access
--   the flag register in hardware.
writeFlag :: SBV Flag -> (SBV Bool) -> Machine ()
writeFlag flag value = do
    delay 1
    modify $ \currentState ->
        currentState {
            flags = writeArray (flags currentState) flag (ite value 1 0)}

--------------------------------------------------------------------------------
------------ Program -----------------------------------------------------------
--------------------------------------------------------------------------------

execute :: Instruction -> Machine ()
execute (Halt                ) = executeHalt
execute (Load     rX dmemaddr) = executeLoad     rX dmemaddr
execute (LoadMI   rX dmemaddr) = executeLoadMI   rX dmemaddr
execute (Set      rX simm    ) = executeSet      rX simm
execute (Store    rX dmemaddr) = executeStore    rX dmemaddr
execute (Add      rX dmemaddr) = executeAdd      rX dmemaddr
execute (Jump     simm       ) = executeJump     simm
execute (JumpZero simm       ) = executeJumpZero simm
-- execute (CmpGT    rx dmemaddr) = executeCmpGT    rx dmemaddr
-- execute (JumpGT   simm       ) = executeJumpGT   simm

executeHalt :: Machine ()
executeHalt = writeFlag (literal Halted) true

executeLoad :: Register -> MemoryAddress -> Machine ()
executeLoad rX dmemaddr = readMemory (literal dmemaddr) >>=
                          writeRegister (literal rX)

executeLoadMI :: Register -> MemoryAddress -> Machine ()
executeLoadMI rX dmemaddr =
    readMemory (literal dmemaddr) >>= toMemoryAddress >>=
    readMemory >>= writeRegister (literal rX)

executeSet :: Register -> Byte -> Machine ()
executeSet rX simm = writeRegister (literal rX) (fromByte . literal $ simm)

executeStore :: Register -> MemoryAddress -> Machine ()
executeStore rX dmemaddr = readRegister (literal rX) >>= writeMemory (literal dmemaddr)

executeAdd :: Register -> MemoryAddress -> Machine ()
executeAdd rX dmemaddr = do
    x <- readRegister (literal rX)
    y <- readMemory (literal dmemaddr)
    let z = x + y
    writeFlag (literal Zero) (z .== 0)
    writeRegister (literal rX) z

executeJump :: Byte -> Machine ()
executeJump simm =
    modify $ \currentState ->
        currentState {instructionCounter =
            instructionCounter currentState + (fromByte . literal $ simm)}

executeJumpZero :: Byte -> Machine ()
executeJumpZero simm = do
    zeroIsSet <- readFlag (literal Zero)
    ic <- instructionCounter <$> get
    let ic' = ite zeroIsSet (ic + (fromByte . literal $ simm)) ic
    modify $ \currentState ->
        currentState {instructionCounter = ic'}

-- executeCmpGT :: Register -> MemoryAddress -> Machine ()
-- executeCmpGT rx dmemaddr = do
--     gt <- (.>) <$> readRegister (literal rx) <*> readMemory (literal dmemaddr)
--     writeFlag (literal Compare) gt

-- executeJumpGT :: Byte -> Machine ()
-- executeJumpGT simm = do
--     isGt <- readFlag (literal Compare)
--     ic <- instructionCounter <$> get
--     let ic' = ite isGt (ic + (fromByte . literal $ simm)) ic
--     modify $ \currentState ->
--         currentState {instructionCounter = ic'}
--------------------------------------------------------------------------------

executeInstruction :: Machine ()
executeInstruction = do
    fetchInstruction
    incrementInstructionCounter
    execute =<< readInstructionRegister

-- | Increment the instruction counter.
incrementInstructionCounter :: Machine ()
incrementInstructionCounter =
    modify $ \currentState ->
        currentState {instructionCounter = instructionCounter currentState + 1}

fetchInstruction :: Machine ()
fetchInstruction =
    get >>= readProgram . instructionCounter >>= writeInstructionRegister

readProgram :: SBV InstructionAddress -> Machine (SBV Instruction)
readProgram addr = do
    currentState <- get
    delay 1
    pure $ readArray (program currentState) addr

readInstructionRegister :: Machine Instruction
readInstructionRegister = do
    i <- unliteral . instructionRegister <$> get
    case i of
        Just i  -> pure i
        Nothing -> error "Error: InstructionRegister became symbolic."

-- readInstructionRegister :: Machine (SBV Instruction)
-- readInstructionRegister = instructionRegister <$> get

writeInstructionRegister :: SBV Instruction -> Machine ()
writeInstructionRegister instruction =
    modify $ \currentState ->
        currentState {instructionRegister = instruction}

--------------------------------------------------------------------------------

fromByte :: SBV Byte -> SBV Value
fromByte s = fromBitsLE $ blastLE s ++ replicate 56 (sTestBit s 7)

