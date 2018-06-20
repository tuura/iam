{-# LANGUAGE StandaloneDeriving, DeriveDataTypeable, DeriveAnyClass #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Machine.Semantics.Symbolic.Types
-- Copyright   :  (c) Georgy Lukyanov, Andrey Mokhov 2017
--
-- Maintainer  :  lukyanov.georgy@gmail.com
-- Stability   :  experimental
--
-- This module contains symbolic versions of the IAM domain types
--
--------------------------------------------------------------------------------
module Machine.Semantics.Symbolic.Types  (
    -- * Types of values operated by the IAM instruction set
    Plain.Value,

    -- * Signed immediate arguments
    Plain.Byte,

    -- * Registers
    Plain.Register (..), RegisterBank,

    -- * Memory
    Plain.MemoryAddress, Memory,

    -- * Flags
    Plain.Flag (..), Flags,

    -- System clock
    Plain.Clock
    ) where

import Data.SBV (HasKind, SymWord, SFunArray)
import qualified Data.Data as Data
import qualified Machine.Types as Plain

-- Symbolic data instances for the 'Register' type
deriving instance Data.Data Plain.Register
deriving instance HasKind Plain.Register
deriving instance SymWord Plain.Register

-- | The register bank is represented by a map from registers to their values.
type RegisterBank = SFunArray Plain.Register Plain.Value

-- | The memory is represented by a map from memory addresses to their values.
type Memory = SFunArray Plain.MemoryAddress Plain.Value

-- Symbolic data instances for the 'Flag' type
deriving instance Data.Data Plain.Flag
deriving instance HasKind Plain.Flag
deriving instance SymWord Plain.Flag

-- | The state of flags is represented by a map from flags to their values.
type Flags = SFunArray Plain.Flag Plain.Value

