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
import Control.Selective
import Machine.Semantics.Symbolic.Types
import Machine.Semantics.Symbolic.State
import Machine.Instruction
import Machine.Value (toValue, fromValue)

instance Monad m => Selective (StateT s m) where

-- | Iam machine is a state transformer
newtype Machine a = Machine { runMachine :: State MachineState a }
    deriving (Functor, Applicative, Monad, MonadState MachineState) -- , Selective)

run :: Machine a -> MachineState -> (a, MachineState)
run = runState . runMachine

instance Selective Machine where
    handle mx mf = do
        x <- mx
        case x of
            Left  a -> fmap ($a) mf
            Right b -> pure b



-- class Applicative f => Selective f where
--     handle :: f (Either a b) -> f (a -> b) -> f b
--     default handle :: Monad f => f (Either a b) -> f (a -> b) -> f b
--     handle = handleM

-- instance Semigroup e => Selective (Validation e) where
--     handle (Success (Right b)) _ = Success b
--     handle (Success (Left  a)) f = Success ($a) <*> f
--     handle (Failure e        ) _ = Failure e

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
    -- return $ fromBitsLE (take 8 $ blastLE value)
    pure value

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
readFlag :: SBV Flag -> Machine (SBV Value)
readFlag flag = do
    currentState <- get
    pure $ readArray (flags currentState) flag

-- | Set a given 'Flag' to the specified Boolean value.
--   We assume that it takes 1 clock cycle to access
--   the flag register in hardware.
writeFlag :: SBV Flag -> (SBV Value) -> Machine ()
writeFlag flag value = do
    delay 1
    modify $ \currentState ->
        currentState {
            flags = writeArray (flags currentState) flag value}

--------------------------------------------------------------------------------
------------ Program -----------------------------------------------------------
--------------------------------------------------------------------------------
-- | Increment the instruction counter.
incrementInstructionCounter :: Machine ()
incrementInstructionCounter =
    modify $ \currentState ->
        currentState {instructionCounter = instructionCounter currentState + 1}

fetchInstruction :: Machine ()
fetchInstruction =
    get >>= readProgram . instructionCounter >>= writeInstructionRegister

readProgram :: SBV InstructionAddress -> Machine (SBV InstructionCode)
readProgram addr = do
    currentState <- get
    delay 1
    pure $ readArray (program currentState) addr

readInstructionRegister :: Machine (SBV InstructionCode)
readInstructionRegister = instructionRegister <$> get

writeInstructionRegister :: SBV (InstructionCode) -> Machine ()
writeInstructionRegister instruction =
    modify $ \currentState ->
        currentState {instructionRegister = instruction}
