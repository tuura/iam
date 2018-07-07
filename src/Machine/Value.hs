{-# LANGUAGE FlexibleInstances, FlexibleContexts, TypeFamilies #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Machine.Semantics.Symbolic
-- Copyright   :  (c) Georgy Lukyanov, Andrey Mokhov 2018
--
-- Maintainer  :  mail@geo2a.info
-- Stability   :  experimental
--
-- Interpret the read-write metalanguage as symbolic execution to verify
-- programs with an SMT solver.
--------------------------------------------------------------------------------
module Machine.Value where

import Control.Selective
import qualified Data.SBV as SBV (true, false, blastLE, fromBitsLE
                                 , (&&&), (|||), (.<), (.>), (.==), ite)
import Data.SBV (SBV, SBool, literal, SFiniteBits, sFiniteBitSize, Boolean (..))
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
    zero     :: a
    overflow :: a
    halted   :: a

instance IsFlag Flag where
    zero     = Zero
    overflow = Overflow
    halted   = Halted

instance IsFlag (SBV Flag) where
    zero     = literal Zero
    overflow = literal Overflow
    halted   = literal Halted

-- class IsBool a where
--     true, false :: a
--     and, or :: a -> a -> a

-- instance IsBool Bool where
--     true  = True
--     false = False
--     and = (&&)
--     or  = (||)

-- instance IsBool SBool where
--     true  = SBV.true
--     false = SBV.false
--     and = (SBV.&&&)
--     or  = (SBV.|||)
--------------------------------------------------------------------------------
type family BoolType a where
    BoolType Value = Bool
    BoolType (SBV Value) = SBool
    BoolType Bool = Bool
    BoolType (SBV Bool) = SBool

class MachineBits a where
    -- type BoolType a
    blastLE :: a -> [BoolType a]
    fromBitsLE :: [BoolType a] -> a

-- Looks like we have to manually declare instances for every datatype to avoid undecidable instances.
-- Since every datatype is just a type alias for 'Value' -- we need only one instance.. 

instance MachineBits Value where
    -- type BoolType Value = Bool
    blastLE    = blastLEImpl
    fromBitsLE = fromBitsLEImpl

instance MachineBits (SBV Value) where
    -- type BoolType (SBV Value) = SBool
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


class MachineEq a where
    eq :: a -> a -> (BoolType a)

instance MachineEq Value where
    eq = (==)

instance MachineEq (SBV Value) where
    eq = (SBV..==)

instance MachineEq Bool where
    eq = (==)

instance MachineEq (SBV Bool) where
    eq = (SBV..==)

class MachineOrd a where
    gt  :: a -> a -> (BoolType a)
    lt  :: a -> a -> (BoolType a)

instance MachineOrd Value where
    gt = (>)
    lt = (<)

instance MachineOrd (SBV Value) where
    gt = (SBV..>)
    lt = (SBV..<)

class ITE a where
    ite :: (BoolType a) -> a -> a -> a

instance ITE Value where
    ite i t e = if i then t else e

instance ITE (SBV Value) where
    ite = SBV.ite

-- | Branch on a Boolean value, skipping unnecessary effects.
-- ifS' :: (Selective f, Boolean b) => f b -> f a -> f a -> f a
-- ifS' i t e = undefined -- select (bool' (Right ()) (Left ()) <$> i) (const <$> t) (const <$> e)

ifS' :: (Selective f, Boolean b) => f b -> f a -> f a -> f a
ifS' i t e = undefined -- select (bool' (Right ()) (Left ()) <$> i) (const <$> t) (const <$> e)

-- bool' :: (Boolean (BoolType a), ITE a) => a -> a -> (BoolType a) -> a
-- -- bool' f t cond | cond == false = f
-- --                | cond == true  = t
-- bool' f t cond = ite cond t f
--------------------------------------------------------------------------------

class ToValue a where
    toValue :: a -> Value

class FromValue a where
    fromValue :: Value -> a

instance ToValue InstructionCode where
    toValue addr = fromBitsLEImpl $ blastLEImpl addr ++ replicate 48 false

instance FromValue InstructionCode where
    fromValue addr = fromBitsLEImpl . take 16 $ blastLEImpl addr