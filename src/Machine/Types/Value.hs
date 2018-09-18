{-# LANGUAGE DeriveAnyClass, TypeSynonymInstances #-}

module Machine.Types.Value where

import Prelude hiding (Monad, read, div, mod, abs, and, or)
import qualified Prelude (Monad, div, mod, abs)
import Machine.Types
import Machine.Semantics.Symbolic.Types

class (Num a, Bounded a) => MachineValue a where
    eq   :: a -> a -> a
    gt   :: a -> a -> a
    lt   :: a -> a -> a
    and  :: a -> a -> a
    or   :: a -> a -> a
    div  :: a -> a -> a
    mod  :: a -> a -> a
    unsafeFromSImm8 :: SImm8 -> a
    unsafeToBool :: a -> Bool
    -- unsafeToSImm8   :: a

instance MachineValue Value where
    eq  x y  = if (x == y) then 1 else 0
    lt  x y  = if (x <  y) then 1 else 0
    gt  x y  = if (x >  y) then 1 else 0
    and x y  = case x > 0 && y > 0 of True  -> 1
                                      False -> 0
    or x y   = case x > 0 || y > 0 of True  -> 1
                                      False -> 0
    div = Prelude.div
    mod = Prelude.mod
    unsafeFromSImm8 = fromIntegral
    unsafeToBool v = v /= 0

instance Num Sym where
    -- Ad-hoc constant folding. Helps to keep the instruction counter concrete.
    (SConst x) + (SConst y) = SConst (x + y)
    x + y      = SAdd x y
    x - y      = SSub x y
    x * y      = error "Sym.Num: * can't be defined for Sym"
    negate x   = SNot x
    abs x         = SAbs x
    signum _      = error "Sym.Num: signum can't be defined for Sym"
    fromInteger x = SConst (fromInteger x)

instance Bounded Sym where
    maxBound = SConst maxBound
    minBound = SConst minBound

instance MachineValue Sym where
    eq   = SEq
    lt   = SLt
    gt   = SGt
    and  = SAnd
    or   = SOr
    div  = SDiv
    mod  = SMod
    unsafeFromSImm8 x = SConst (fromIntegral x)
    unsafeToBool (SConst v) = v /= 0
    unsafeToBool _          = SNot (SEq v 0)
        error "Sym.MachineValue: unsafeToBool can't be defined for non-concrete values"
