{-# LANGUAGE DeriveDataTypeable,
             DeriveAnyClass,
             DeriveGeneric #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Machine.State
-- Copyright   :  (c) Georgy Lukyanov, Andrey Mokhov 2017
--
-- Maintainer  :  lukyanov.georgy@gmail.com
-- Stability   :  experimental
--
-- The domain types for the entities of the Inglorious adding machine.
--
--------------------------------------------------------------------------------
module Machine.Types where

import Prelude hiding (Monad, read)
import qualified Prelude (Monad)
import qualified GHC.Generics as GHC
import qualified Data.Data as Data
import Control.Monad.State hiding (Monad)
import Data.SBV
import Data.Maybe (fromJust)

-- | The 'Value' datatype represents data values. The precise
-- bit-width is left unspecified, but it is assumed that it fits into 64 bits.
type Value = Word64

-- | The 'SImm8' datatype represents 8-bit signed immediate arguments that are
-- used by many Iam instructions with immediate addressing mode.
type SImm8 = Int8

-- | The 'SImm10' datatype represents 10-bit signed immediate arguments that are
-- used for specifying the relative jump address, e.g. in
-- 'Iam.Assembly.Assembly.Jump' instruction.
type SImm10 = Int16

-- | Iam has 4 general-purpose registers.
data Register = R0 | R1 | R2 | R3
    deriving (Eq, Ord, Show, Read, Data.Data, HasKind, SymWord)

-- | The register bank is represented by a map from registers to their values.
type RegisterBank = SFunArray Register Value

-- | Iam memory can hoload 256 values.
type MemoryAddress = Word8

-- | The memory is represented by a map from memory addresses to their values.
type Memory = SFunArray MemoryAddress Value

-- | Boolean 'Flag's indicate the current status of Iam.
data Flag = Zero
          | Compare
          | Halted
          deriving (Eq, Ord, Show, Read, Data.Data, HasKind, SymWord)

-- | The state of flags is represented by a map from flags to their values.
type Flags = SFunArray Flag Bool

-- | 'Clock' is the current time measured in clock cycles. It used to model the
-- effect of the 'Iam.Semantics.wait' instruction.
type Clock = Word64

-- | Iam instructions
data Instruction = Halt
                 | Load     Register MemoryAddress
                 | LoadMI   Register MemoryAddress
                 | Set      Register SImm8
                 | Store    Register MemoryAddress
                 | Add      Register MemoryAddress
                 | CmpGT    Register MemoryAddress
                 | JumpGT   SImm10
                 | Jump     SImm10
                 | JumpZero SImm10
    deriving (Eq, Ord, Show, Read, Data.Data, HasKind, SymWord)

-- | Programs are stored in program memory (currently, up to 1024 instructions).
type InstructionAddress = Word16

-- | The program is represented by a map from instruction addresses to
--   instructions.
type Program = SFunArray InstructionAddress Instruction
