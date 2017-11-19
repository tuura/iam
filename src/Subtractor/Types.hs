-- {-# LANGUAGE StandaloneDeriving #-}
-- {-# LANGUAGE FlexibleInstances #-}
-- {-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Subtractor.Types where

import Data.SBV
import GHC.Generics
import Data.Map
import Control.Monad.State.Strict
import Control.Monad.Writer.Strict

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

type Program = [(InstructionAddress, Instruction)]

-- | Programs are stored in program memory (currently, up to 1024 instructions).
type InstructionAddress = SWord16

-- | Instructions have 16-bit codes.
type InstructionCode = SWord16

-- | Boolean 'Flag's indicate the current status of Subtractor.
data Flag = Condition
          | Halted
          deriving (Enum, Eq, Ord, Show)

flagId :: Flag -> SWord8
flagId = literal . fromIntegral . fromEnum

-- | The state of flags is represented by a map from flags to their values.
type Flags = SFunArray Word8 Bool

-- | 'Clock' is the current time measured in clock cycles. It used to model the
-- effect of the 'Subtractor.Semantics.wait' instruction.
type Clock = SWord64

--------------------------------------------------------------------------------

data Instruction = Halt
                 | Ld    Register MemoryAddress
                 | Ld_si Register SImm8
                 | St    Register MemoryAddress
                 | Sub   Register MemoryAddress
                 | Jmpi  SImm10
    deriving (Show, Eq)

instance Mergeable Instruction where
    symbolicMerge f t Halt Halt = Halt
    symbolicMerge f t (Ld    rX1 dmemaddr1) (Ld    rX2 dmemaddr2) =
        Ld (symbolicMerge f t rX1 rX2) (symbolicMerge f t dmemaddr1 dmemaddr2)
    symbolicMerge f t (Ld_si rX1 simm1    ) (Ld_si rX2 simm2)     =
        Ld_si (symbolicMerge f t rX1 rX2) (symbolicMerge f t simm1 simm2)
    symbolicMerge f t (St    rX1 dmemaddr1) (St    rX2 dmemaddr2) =
        St (symbolicMerge f t rX1 rX2) (symbolicMerge f t dmemaddr1 dmemaddr2)
    symbolicMerge f t (Sub   rX1 dmemaddr1) (Sub   rX2 dmemaddr2) =
        Sub (symbolicMerge f t rX1 rX2) (symbolicMerge f t dmemaddr1 dmemaddr2)
    symbolicMerge f t (Jmpi  simm1        ) (Jmpi  simm2)         =
        Jmpi (symbolicMerge f t simm1 simm2)
    symbolicMerge _ _ a b =
        error $ "Subtractor.Types: No least-upper-bound for " ++ show (a, b)
--------------------------------------------------------------------------------

type Script = Writer [Instruction] ()

ld :: Register -> MemoryAddress -> Script
ld rX dmemaddr = tell [Ld rX dmemaddr]

ld_si :: Register -> SImm8 -> Script
ld_si rX simm = tell [Ld_si rX simm]

st :: Register -> MemoryAddress -> Script
st rX dmemaddr = tell [St rX dmemaddr]

sub :: Register -> MemoryAddress -> Script
sub rX dmemaddr = tell [Sub rX dmemaddr]

jmpi :: SImm10 -> Script
jmpi simm = tell [Jmpi simm]

halt :: Script
halt = tell [Halt]

assemble :: Script -> Program
assemble s = zip [0..] prg
  where
    prg = snd $ runWriter s

data MachineState = MachineState
    { registers           :: RegisterBank
    , instructionCounter  :: InstructionAddress
    , instructionRegister :: Instruction
    , flags               :: Flags
    , memory              :: Memory
    , program             :: Program
    , clock               :: Clock
    } deriving (Show, Generic, Mergeable)
