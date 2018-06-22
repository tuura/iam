{-# LANGUAGE StandaloneDeriving, DeriveDataTypeable, DeriveAnyClass, FlexibleInstances #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Machine.Semantics.Symbolic.Instruction
-- Copyright   :  (c) Georgy Lukyanov, Andrey Mokhov 2017
--
-- Maintainer  :  lukyanov.georgy@gmail.com
-- Stability   :  experimental
--
-- This module contains symbolic versions of the IAM instructions
--
--------------------------------------------------------------------------------
module Machine.Semantics.Symbolic.Instruction  (
        -- * Instruction syntax and related types
    Plain.Instruction (..), Plain.InstructionAddress,

    -- * Program type and program parser
    Program,
    ) where

import Data.SBV (SBV, HasKind, SymWord, SFunArray)
import qualified Data.Data as Data
import Machine.Semantics.Symbolic.Types (Register (..), MemoryAddress, Flag, Byte)
import qualified Machine.Instruction as Plain

-- Symbolic data instances for the 'Instruction' type
deriving instance Data.Data (Plain.Instruction Register MemoryAddress Flag Byte)
deriving instance HasKind (Plain.Instruction Register MemoryAddress Flag Byte)
deriving instance SymWord (Plain.Instruction Register MemoryAddress Flag Byte)

-- | The program is represented by a map from instruction addresses to
--   instructions.
type Program = SFunArray Plain.InstructionAddress
    (Plain.Instruction Register MemoryAddress Flag Byte)

