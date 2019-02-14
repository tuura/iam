{-# LANGUAGE DeriveAnyClass #-}
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
    MachineValue,

    -- * Signed immediate arguments
    SImm8,

    -- * Registers
    Register (..), RegisterBank,

    -- * Memory
    MemoryAddress, Memory, toMemoryAddress,

    -- * Flags
    Flag (..), Flags, FlagId, flagId,

    -- System clock
    Clock,

    -- Numbers to [Bool] conversion
    fromBitsLE, blastLE
    ) where

import qualified Data.Map.Strict as Map
import Data.Word (Word8, Word16, Word64)
import Data.Int (Int8, Int16, Int64)
import Data.Bits

-- | The 'MachineValue' datatype represents data values. The precise
-- bit-width is left unspecified, but it is assumed that it fits into 64 bits.
type MachineValue = Int16

type SImm8 = Int8

data Register = R0 | R1 | R2 | R3
    deriving (Show, Read, Eq, Ord, Enum)

-- | The register bank is represented by a map from registers to their values.
type RegisterBank = Map.Map Register MachineValue

type MemoryAddress = MachineValue

-- | The memory is represented by a map from memory addresses to their values.
type Memory = Map.Map MemoryAddress MachineValue

-- | Boolean 'Flag's indicate the current status of Iam.
data Flag = Zero
          | Overflow
          | Halted
          deriving (Show, Read, Eq, Ord, Enum)

type FlagId = MachineValue

flagId :: Flag -> FlagId
flagId = fromIntegral . fromEnum

-- | The state of flags is represented by a map from flags to their values.
type Flags = Map.Map Flag Bool

-- | 'Clock' is the current time measured in clock cycles. It used to model the
-- effect of the 'Iam.Semantics.wait' instruction.
type Clock = MachineValue

--------------------------------------------------------------------------------
fromBitsLE :: (FiniteBits a, Num a) => [Bool] -> a
fromBitsLE = go 0 0
  where go acc _  []     = acc
        go acc i (x:xs) = go (if x then (setBit acc i) else acc) (i+1) xs

blastLE :: FiniteBits a => a -> [Bool]
blastLE x = map (testBit x) [0 .. finiteBitSize x - 1]

toMemoryAddress :: MachineValue -> MemoryAddress
toMemoryAddress = id