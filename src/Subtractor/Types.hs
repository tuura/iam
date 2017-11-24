--------------------------------------------------------------------------------
-- |
-- Module      :  Subtractor.State
-- Copyright   :  (c) Georgy Lukyanov, Andrey Mokhov 2017
--
-- Maintainer  :  lukyanov.georgy@gmail.com
-- Stability   :  experimental
--
-- The domain types of SUBTRACTOR.
--
--------------------------------------------------------------------------------
module Subtractor.Types where

import Data.SBV
import GHC.Generics
import Control.Monad.State.Strict

-- | The 'Value' datatype represents data values. The precise
-- bit-width is left unspecified, but it is assumed that it fits into 64 bits.
type Value = SWord64

-- | The 'SImm8' datatype represents 8-bit signed immediate arguments that are
-- used by many Subtractor instructions with immediate addressing mode.
type SImm8 = SInt8

-- | The 'SImm10' datatype represents 10-bit signed immediate arguments that are
-- used for specifying the relative jump address, e.g. in
-- 'Subtractor.Assembly.Assembly.Jump' instruction.
type SImm10 = SInt16

-- | Subtractor has 4 general-purpose registers.
type Register = SWord8

-- | The register bank is represented by a map from registers to their values.
type RegisterBank = SFunArray Word8 Word64

-- | Subtractor memory can hoload 256 values.
type MemoryAddress = SWord8

-- | The memory is represented by a map from memory addresses to their values.
type Memory = SFunArray Word8 Word64

-- | Boolean 'Flag's indicate the current status of Subtractor.
data Flag = Zero
          | Halted
          deriving (Enum, Eq, Ord, Show)

flagId :: Flag -> SWord8
flagId = literal . fromIntegral . fromEnum

-- | The state of flags is represented by a map from flags to their values.
type Flags = SFunArray Word8 Bool

-- | 'Clock' is the current time measured in clock cycles. It used to model the
-- effect of the 'Subtractor.Semantics.wait' instruction.
type Clock = SWord64

-- | Subtractor instructions
data Instruction = Halt
                 | Load     Register MemoryAddress
                 | LoadMI   Register MemoryAddress
                 | Set      Register SImm8
                 | Store    Register MemoryAddress
                 | Subtract Register MemoryAddress
                 | Jump     SImm10
                 | JumpZero SImm10
    deriving (Show, Eq)

instance Mergeable Instruction where
    symbolicMerge _ _ Halt Halt = Halt
    -- symbolicMerge _ _ _ Halt = Halt
    -- symbolicMerge _ _ Halt _ = Halt
    symbolicMerge f t (Load rX1 dmemaddr1) (Load rX2 dmemaddr2) =
        Load (symbolicMerge f t rX1 rX2) (symbolicMerge f t dmemaddr1 dmemaddr2)
    symbolicMerge f t (LoadMI rX1 dmemaddr1) (LoadMI rX2 dmemaddr2) =
        LoadMI (symbolicMerge f t rX1 rX2) (symbolicMerge f t dmemaddr1 dmemaddr2)
    symbolicMerge f t (Set rX1 simm1) (Set rX2 simm2)     =
        Set (symbolicMerge f t rX1 rX2) (symbolicMerge f t simm1 simm2)
    symbolicMerge f t (Store rX1 dmemaddr1) (Store rX2 dmemaddr2) =
        Store (symbolicMerge f t rX1 rX2) (symbolicMerge f t dmemaddr1 dmemaddr2)
    symbolicMerge f t (Subtract rX1 dmemaddr1) (Subtract rX2 dmemaddr2) =
        Subtract (symbolicMerge f t rX1 rX2) (symbolicMerge f t dmemaddr1 dmemaddr2)
    symbolicMerge f t (Jump simm1) (Jump simm2) =
        Jump (symbolicMerge f t simm1 simm2)
    symbolicMerge f t (JumpZero simm1) (JumpZero simm2) =
        JumpZero (symbolicMerge f t simm1 simm2)
    symbolicMerge _ _ a b =
        error $ "Subtractor.Types: No least-upper-bound for " ++ show (a, b)

-- | Programs are stored in program memory (currently, up to 1024 instructions).
type InstructionAddress = SWord16

-- | The program is represented by a map from instruction addresses to
--   instructions.
type Program = [(InstructionAddress, Instruction)]
