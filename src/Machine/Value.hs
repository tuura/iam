{-# LANGUAGE FlexibleInstances, TypeFamilies #-}
module Machine.Value where

import Data.SBV (SBV, SBool, literal, SFiniteBits, sFiniteBitSize)
import qualified Data.SBV as SBV (true, false, blastLE, fromBitsLE)
import Data.Bits
import Machine.Types
import Machine.Instruction (InstructionAddress, InstructionCode)
import Machine.Semantics.Symbolic.Types

class IsRegister a where
    r0, r1, r2, r3 :: a

instance IsRegister Register where
    r0 = R0
    r1 = R1
    r2 = R2
    r3 = R3
instance IsRegister (SBV Register) where
    r0 = literal R0
    r1 = literal R1
    r2 = literal R2
    r3 = literal R3

class IsMemoryAddress a where

instance IsMemoryAddress MemoryAddress where
instance IsMemoryAddress (SBV MemoryAddress) where

class IsByte a where

instance IsByte Byte where
instance IsByte (SBV Byte) where

class IsInstructionAddress a where

instance IsInstructionAddress (InstructionAddress) where
instance IsInstructionAddress (SBV InstructionAddress) where

class IsInstructionCode a where

instance IsInstructionCode (InstructionCode) where
instance IsInstructionCode (SBV InstructionCode) where

class IsFlag a where
    zero   :: a
    halted :: a

instance IsFlag Flag where
    zero   = Zero
    halted = Halted

instance IsFlag (SBV Flag) where
    zero   = literal Zero
    halted = literal Halted

class IsBool a where
    true, false :: a

instance IsBool Bool where
    true  = True
    false = False

instance IsBool SBool where
    true  = SBV.true
    false = SBV.false
--------------------------------------------------------------------------------
class MachineBits a where
    type BoolType a
    blastLE :: a -> [BoolType a]
    fromBitsLE :: [BoolType a] -> a

-- Looks like we have to manually declare instances for every datatype to avoid undecidable instances.
-- Since every datatype is just a type alias for 'Value' -- we need only one instance.. 

instance MachineBits Value where
    type BoolType Value = Bool
    blastLE    = blastLEImpl
    fromBitsLE = fromBitsLEImpl

instance MachineBits (SBV Value) where
    type BoolType (SBV Value) = SBool
    blastLE    = SBV.blastLE
    fromBitsLE = SBV.fromBitsLE

blastLEImpl :: (Num a, FiniteBits a) => a -> [Bool]
blastLEImpl x = map (testBit x) [0 .. finiteBitSize x - 1]

fromBitsLEImpl :: (Num a, FiniteBits a) => [Bool] -> a
fromBitsLEImpl bs
    | length bs /= w
        = error $ "IAM.fromBitsLE: Expected: " ++ show w ++ " bits, received: " ++ show (length bs)
    | True
        = result
    where w = finiteBitSize result
          result = go 0 0 bs

          go acc _  []    = acc
          go acc i (x:xs) = go (if x then (setBit acc i) else acc) (i+1) xs
