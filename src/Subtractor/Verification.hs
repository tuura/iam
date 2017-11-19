{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Subtractor.Verification where

import Control.Monad.State.Strict
import Data.SBV
import Subtractor.Types


newtype Machine a = Machine { runMachine :: State MachineState a }
    deriving (Functor, Applicative, Monad, MonadState MachineState)

run :: Machine a -> MachineState -> (a, MachineState)
run = runState . runMachine

-- | Advance the clock by a given number of clock cycles.
delay :: Clock -> Machine ()
delay cycles =
    modify $ \currentState -> currentState {clock = clock currentState + cycles}

--------------------------------------------------------------------------------
------------ Memory ------------------------------------------------------------
--------------------------------------------------------------------------------

-- | Write a new 'Value' to the given 'MemoryAddress'. We assume that it takes 2
-- clock cycles to access the memory in hardware.
writeMemory :: MemoryAddress -> Value -> Machine ()
writeMemory address value = do
    delay 2
    modify $ \currentState ->
        currentState {memory = writeArray (memory currentState) address value}

-- | Lookup the 'Value' at the given 'MemoryAddress'. If the value has never
-- been initialised, this function returns 0, which is how the current hardware
-- implementation works. To handle more general settings, it may also be useful
-- to raise an error flag in this situation (future work). We assume that it
-- takes 2 clock cycles to access the memory in hardware.
readMemory :: MemoryAddress -> Machine Value
readMemory address = do
    currentState <- get
    delay 2
    return $ readArray (memory currentState) address

--------------------------------------------------------------------------------
------------ Registers ---------------------------------------------------------
--------------------------------------------------------------------------------

-- | Lookup the 'Value' in a given 'Register'. If the register has never been
-- initialised, this function returns 0, which is how the current hardware
-- implementation works. To handle more general settings, it may also be useful
-- to raise an error flag in this situation (future work). We assume that it
-- takes 1 clock cycles to access a register in hardware.
readRegister :: Register -> Machine Value
readRegister register = do
    s <- get
    delay 1
    return $ readArray (registers s) register

-- | Write a new 'Value' to a given 'Register'.
--   We assume that it
--   takes 1 clock cycle to access a register in hardware.
writeRegister :: Register -> Value -> Machine ()
writeRegister register value = do
    delay 1
    modify $ \currentState ->
        currentState {registers = writeArray (registers currentState) register value}

--------------------------------------------------------------------------------
------------ Flags ---------------------------------------------------------
--------------------------------------------------------------------------------

-- | Set a given 'Flag' to the specified Boolean value.
--   We assume that it takes 1 clock cycle to access
--   the flag register in hardware.
writeFlag :: Flag -> SBool -> Machine ()
writeFlag flag value = do
    delay 1
    modify $ \currentState ->
        currentState {
            flags = writeArray (flags currentState) (flagId flag) value}

--------------------------------------------------------------------------------
------------ Instruction counter -----------------------------------------------
--------------------------------------------------------------------------------

-- | Increment the instruction counter.
incrementInstructionCounter :: Machine ()
incrementInstructionCounter =
    modify $ \currentState ->
        currentState {instructionCounter = instructionCounter currentState + 1}