{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Machine.Semantics.Symbolic.State
-- Copyright   :  (c) Georgy Lukyanov, Andrey Mokhov 2017
--
-- Maintainer  :  lukyanov.georgy@gmail.com
-- Stability   :  experimental
--
-- The state of the Inglorious Adding Machine.
--
--------------------------------------------------------------------------------
module Machine.Semantics.Symbolic.State (
    MachineState (..), templateState
    ) where

import GHC.Generics (Generic)
import Data.SBV (SBV, literal, mkSFunArray, writeArray, Mergeable)
import Machine.Semantics.Symbolic.Types
import Machine.Semantics.Symbolic.Instruction

-- | The state of a Iam machine
data MachineState = MachineState
    { registers           :: RegisterBank
    , instructionCounter  :: SBV InstructionAddress
    , instructionRegister :: SBV Instruction
    , flags               :: Flags
    , memory              :: Memory
    , program             :: Program
    , clock               :: SBV Clock
    } deriving (Show, Generic, Mergeable)

emptyRegisters :: RegisterBank
emptyRegisters = mkSFunArray $ const 0

emptyFlags :: Flags
emptyFlags = mkSFunArray $ const 0

initialiseMemory :: [(SBV MemoryAddress, SBV Value)] -> Memory
initialiseMemory =
    foldr (\(a, v) m -> writeArray m a v) (mkSFunArray $ const 0)

templateState :: Program -> Memory -> MachineState
templateState prog mem = MachineState { registers = emptyRegisters
                                      , instructionCounter = 0
                                      , instructionRegister = literal $ Jump 0
                                      , program = prog
                                      , flags = emptyFlags
                                      , memory = mem
                                      , clock = 0
                                      }