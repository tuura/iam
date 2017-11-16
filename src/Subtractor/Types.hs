{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

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
-- 'Subtractor.Assembly.Assembly.Jmpi' instruction.
type SImm10 = SInt16

-- | Subtractor has 4 general-purpose registers.
type Register = SWord8

-- | The register bank is represented by a map from registers to their values.
type RegisterBank = SFunArray Word8 Word64

-- | Subtractor memory can hold 256 values.
type MemoryAddress = SWord8

-- | The memory is represented by a map from memory addresses to their values.
type Memory = SFunArray Word8 Word64

-- | Boolean 'Flag's indicate the current status of Subtractor.
data Flag = Condition
          | Halted
          deriving (Enum, Eq, Ord, Show)

-- | Programs are stored in program memory (currently, up to 1024 instructions).
type InstructionAddress = SWord16

flagId :: Flag -> SWord8
flagId = literal . fromIntegral . fromEnum

-- | The state of flags is represented by a map from flags to their values.
type Flags = SFunArray Word8 Bool

-- | 'Clock' is the current time measured in clock cycles. It used to model the
-- effect of the 'Subtractor.Semantics.wait' instruction.
type Clock = SWord64

data MachineState = MachineState
    { registers           :: RegisterBank
    , instructionCounter  :: InstructionAddress
    , flags               :: Flags
    , memory              :: Memory
    , clock               :: Clock
    } deriving (Show)

newtype Machine a = Machine { runMachine :: State MachineState a }
    deriving (Functor, Applicative, Monad, MonadState MachineState)

execute :: Machine a -> MachineState -> (a, MachineState)
execute = runState . runMachine