{-# LANGUAGE FlexibleInstances, TypeSynonymInstances #-}
module Machine.Value where

import Data.SBV (SBV, literal)
import Machine.Types
import Machine.Instruction (InstructionAddress)
import Machine.Semantics.Symbolic.Types

class IsRegister a where

instance IsRegister Register where
instance IsRegister (SBV Register) where

class IsMemoryAddress a where

instance IsMemoryAddress MemoryAddress where
instance IsMemoryAddress (SBV MemoryAddress) where

class IsInstructionAddress a where

instance IsInstructionAddress (InstructionAddress) where
instance IsInstructionAddress (SBV InstructionAddress) where

class IsFlag a where
    zero   :: a
    halted :: a

instance IsFlag Flag where
    zero   = Zero
    halted = Halted

instance IsFlag (SBV Flag) where
    zero   = literal Zero
    halted = literal Halted
