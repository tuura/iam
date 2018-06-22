{-# LANGUAGE FlexibleInstances, TypeSynonymInstances #-}
module Machine.Value where

import Data.SBV (SBV, literal)
import Machine.Types
import Machine.Semantics.Symbolic.Types

class IsRegister a where

instance IsRegister Register where
instance IsRegister (SBV Register) where

class IsMemoryAddress a where

instance IsMemoryAddress MemoryAddress where
instance IsMemoryAddress (SBV MemoryAddress) where

class IsFlag a where
    zero   :: a
    halted :: a

instance IsFlag Flag where
    zero   = Zero
    halted = Halted

instance IsFlag (SBV Flag) where
    zero   = literal Zero
    halted = literal Halted
