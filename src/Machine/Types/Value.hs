{-# LANGUAGE DeriveAnyClass, TypeSynonymInstances #-}

module Machine.Types.Value where

import Machine.Types
import Machine.Semantics.Symbolic.Types

class (Num a, Bounded a) => MachineValue a where
    eq   :: a -> a -> a
    gt   :: a -> a -> a
    lt   :: a -> a -> a
    and  :: a -> a -> a
    or   :: a -> a -> a
    unsafeFromSImm8 :: SImm8 -> a
    -- unsafeToSImm8   :: a

instance MachineValue Value where
    eq  x y  = if (x == y) then 1 else 0
    lt  x y  = if (x <  y) then 1 else 0
    gt  x y  = if (x >  y) then 1 else 0
    and x y  = case x > 0 && y > 0 of True  -> 1
                                      False -> 0
    or x y   = case x > 0 || y > 0 of True  -> 1
                                      False -> 0
    unsafeFromSImm8 = fromIntegral

instance Num Sym where
    x + y      = SAdd x y
    x - y      = SSub x y
    x * y      = error "Sym.Num: * can't be defined for Sym"
    negate x   = SNot x
    abs x         = error "Sym.Num: abs can't be defined for Sym"
    signum _      = error "Sym.Num: signum can't be defined for Sym"
    fromInteger x = SConst (fromInteger x)

instance Bounded Sym where
    maxBound = SConst maxBound
    minBound = SConst minBound

instance MachineValue Sym where
    eq  x y  = SEq x y
    lt  x y  = SLt x y
    gt  x y  = SGt x y
    and x y  = SAnd x y
    or x y   = SOr x y
    unsafeFromSImm8 x = SConst (fromIntegral x)
