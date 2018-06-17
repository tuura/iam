--------------------------------------------------------------------------------
-- |
-- Module      :  Machine.Types
-- Copyright   :  (c) Georgy Lukyanov, Andrey Mokhov 2017
--
-- Maintainer  :  lukyanov.georgy@gmail.com
-- Stability   :  experimental
--
-- The domain types for the entities of the Inglorious adding machine.
--
--------------------------------------------------------------------------------
module Machine.Types (
    -- * Types of values operated by the IAM instruction set
    Value,

    -- * Signed immediate arguments
    SImm8, SImm10,

    -- * Registers
    Register (..), RegisterBank,

    -- * Memory
    MemoryAddress, Memory,

    -- * Flags
    Flag (..), Flags, FlagId, flagId,

    -- System clock
    Clock
    ) where

import qualified Data.Map.Strict as Map

-- | The 'Value' datatype represents data values. The precise
-- bit-width is left unspecified, but it is assumed that it fits into 64 bits.
type Value = Int

type SImm8 = Value

type SImm10 = Value

data Register = R0 | R1 | R2 | R3
    deriving (Show, Read, Eq, Ord, Enum)

-- | The register bank is represented by a map from registers to their values.
type RegisterBank = Map.Map Register Value

type MemoryAddress = Value

-- | The memory is represented by a map from memory addresses to their values.
type Memory = Map.Map MemoryAddress Value

-- | Boolean 'Flag's indicate the current status of Iam.
data Flag = Zero
          | Halted
          deriving (Enum, Eq, Ord, Show)

type FlagId = Value

flagId :: Flag -> FlagId
flagId = fromIntegral . fromEnum

-- | The state of flags is represented by a map from flags to their values.
type Flags = Map.Map Flag Value

-- | 'Clock' is the current time measured in clock cycles. It used to model the
-- effect of the 'Iam.Semantics.wait' instruction.
type Clock = Value
