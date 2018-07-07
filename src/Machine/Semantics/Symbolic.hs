{-# LANGUAGE FlexibleInstances, RankNTypes, GADTs, LambdaCase #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Machine.Semantics.Symbolic
-- Copyright   :  (c) Georgy Lukyanov, Andrey Mokhov 2018
--
-- Maintainer  :  mail@geo2a.info
-- Stability   :  experimental
--
-- Interpret the read-write metalanguage as symbolic execution to verify
-- programs with an SMT solver.
--------------------------------------------------------------------------------
module Machine.Semantics.Symbolic where

import Data.SBV
import Data.Maybe (fromJust)
import Control.Monad.State (get, modify)
import Machine.Semantics
import Machine.Instruction
import Machine.Semantics.Symbolic.Types
import Machine.Semantics.Symbolic.State
import Machine.Semantics.Symbolic.Machine

import Machine.Value (toValue, fromValue)
import Machine.Instruction.Encode
import Machine.Instruction.Decode

-- | Run symbolic execution using the provided initial state and
--   the maximum number of state transitions
runModel :: Int -> MachineState -> MachineState
runModel steps state
    | steps == 0 = state
    | otherwise  = ite halted state (runModel (steps - 1) nextState)
  where
    halted    = (./= 0) $ readArray (flags state) (literal Halted)
    nextState =
        case executeInstruction readKey writeKey of
            Just computation -> snd $ run computation state
            Nothing -> error
                "Machine.Semantics.Symbolic.runModel: no semantics for the next instruction."

-- | Instance of the Machine.Metalanguage read command for symbolic execution
readKey :: MachineKey (SBV Register) (SBV MemoryAddress) (SBV InstructionAddress) (SBV Flag)
        -> Machine (SBV Value)
readKey = \case
    Reg  reg  -> readRegister reg
    Addr addr -> readMemory   addr
    F    flag -> readFlag     flag
    IC        -> instructionCounter <$> get
    IR        -> readInstructionRegister
    Prog addr -> readProgram addr

-- | Instance of the Machine.Metalanguage write command for symbolic execution
writeKey :: MachineKey (SBV Register) (SBV MemoryAddress) (SBV InstructionAddress) (SBV Flag)
         -> Machine (SBV Value)
         -> Machine ()
writeKey k v = case k of
    Reg  reg  -> v >>= writeRegister reg
    Addr addr -> v >>= writeMemory   addr
    F    flag -> v >>= writeFlag flag
    IC        -> do
        ic' <- v
        modify $ \currentState -> currentState {instructionCounter = ic'}
    IR        -> v >>= writeInstructionRegister
    Prog addr -> error "Machine.Semantics.Symbolic: Can't write Program"

-- | Assemble a list of instructions into a symbolic execution-ready program
assemble :: [Instruction Register MemoryAddress Flag Byte] -> Program
assemble s = foldr (\(c, p) a -> writeArray a p c) a0 (zip (map literal prg) [0..])
  where
    a0  = mkSFunArray (const $ literal 0)
    prg = map encode $ s

-- An alternative to defining these orphan instances is to switch to SBV's type
-- class SDivisible instead. Even better is to fix Haskell's class hierarchy.
instance Integral (SBV Int64) where
    div       = sDiv
    quotRem   = error "quotRem is not implemented for SBV Int64"
    toInteger = error "quotRem cannot be implemented for SBV Int64"

instance Ord (SBV Int64) where
    compare = error "Ord cannot be implemented for SBV Int64"

instance Real (SBV Int64) where
    toRational = error "Real cannot be implemented for SBV Int64"