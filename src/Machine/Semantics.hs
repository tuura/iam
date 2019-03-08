{-# LANGUAGE ConstraintKinds, RankNTypes,
             ScopedTypeVariables,
             FlexibleContexts,
             FlexibleInstances,
             TypeApplications,
             TypeFamilies,
             GADTs,
             StandaloneDeriving,
             MultiWayIf,
             LambdaCase #-}

module Machine.Semantics where

import Prelude hiding (Monad, read, div, mod, abs, and, or)
import qualified Prelude (Monad, div, mod, abs)
import Data.Maybe (fromJust)
import Control.Monad (join)
import Data.SBV hiding (Boolean)
import Metalanguage
import Machine.Types
import Machine.Types.Value hiding (div, mod)
import qualified Machine.Types.Value as Value (div, mod)
import Machine.Instruction
import Machine.Instruction.Encode
import Machine.Instruction.Decode
import Control.Selective

-- | 'MachineKey' will instantiate the 'k' type variable in the 'Semantics'
--   metalanguage.
-- 'r' is the register type
-- 'addr' is the memory address type
-- 'iaddr' is the instruction address type
-- 'flag' is the flag type
data MachineKey where
    Reg  :: Register -> MachineKey
    -- ^ register
    Addr :: MemoryAddress -> MachineKey
    -- ^ memory address
    F    :: Flag -> MachineKey
    -- ^ flag
    IC   :: MachineKey
    -- ^ instruction counter
    IR   :: MachineKey
    -- ^ instruction register
    Prog :: InstructionAddress -> MachineKey
    -- ^ program memory address

deriving instance Show MachineKey

-- | We amend the standard 'Monad' constraint to include 'Selective' into
--   the hierarchy
type Monad m = (Selective m, Prelude.Monad m)

-- | Functorial semantics is data independent and may have a most one
--   static dependency. May be used for
--   static analysis.
semanticsF :: MachineValue a => Instruction
                             -> Semantics Functor MachineKey a ()
semanticsF = \case
    Halt           -> haltF
    Load reg addr  -> load reg addr
    LoadMI _ _     -> const (const Nothing)
    Set reg simm8  -> setF reg simm8
    Store reg addr -> store reg addr
    Add _ _        -> const (const Nothing)
    Sub _ _        -> const (const Nothing)
    Mul _ _        -> const (const Nothing)
    Div _ _        -> const (const Nothing)
    Mod _ _        -> const (const Nothing)
    Abs reg        -> abs reg
    Jump simm8     -> jump simm8
    JumpZero simm8 -> const (const Nothing)

-- | Applicative semantics is data independent. May be used for
--   static code analysis.
--
--   Note: applicative semantics cannot interact with flags.
semanticsA :: MachineValue a => Instruction
                             -> Semantics Applicative MachineKey a ()
semanticsA i = case i of
    Halt           -> haltA
    Load   _ _     -> semanticsF i
    LoadMI _ _     -> const (const Nothing)
    Set reg simm8  -> setA reg simm8
    Store  _ _     -> semanticsF i
    Add reg addr   -> add reg addr
    Sub reg addr   -> sub reg addr
    Mul reg addr   -> mul reg addr
    Div reg addr   -> div reg addr
    Mod reg addr   -> mod reg addr
    Abs _          -> semanticsF i
    Jump _         -> semanticsF i
    JumpZero simm8 -> const (const Nothing)

semanticsS :: MachineValue a => Instruction
                             -> Semantics Selective MachineKey a ()
semanticsS i = case i of
    Halt           -> semanticsA i
    Load   _ _     -> semanticsA i
    LoadMI _ _     -> const (const Nothing)
    Set reg simm8  -> setA reg simm8
    Store  _ _     -> semanticsA i
    Add reg addr   -> addS reg addr
    Sub reg addr   -> semanticsA i
    Mul reg addr   -> semanticsA i
    Div reg addr   -> semanticsA i
    Mod reg addr   -> semanticsA i
    Abs _          -> semanticsA i
    Jump _         -> semanticsA i
    JumpZero simm8 -> jumpZero simm8

-- | Monadic semantics may involve data dynamic analysis and must be executed
--   on a concrete machine state.
--
--   Note: Indirect memory access ('LoadMI') and conditional jump ('JumpZero')
--   instruction may be only assigned monadic semantics.
semanticsM :: MachineValue a => Instruction
                             -> Semantics Monad MachineKey a ()
semanticsM i = case i of
    Halt            -> semanticsS i
    Load   _ _      -> semanticsS i
    LoadMI reg addr -> undefined -- loadMI reg addr
    Set reg simm8   -> setA reg simm8
    Store  _ _      -> semanticsS i
    Add reg addr    -> addS reg addr
    Sub reg addr    -> semanticsS i
    Mul reg addr    -> semanticsS i
    Div reg addr    -> semanticsS i
    Mod reg addr    -> semanticsS i
    Abs _           -> semanticsS i
    Jump _          -> semanticsS i
    JumpZero simm8  -> jumpZero simm8

-- | Halt the execution.
--   Functor.
haltF :: MachineValue a => Semantics Functor MachineKey a ()
haltF read write = Just $
    write (F Halted) ((const 1) <$> read (F Halted))

-- | Halt the execution.
--   Applicative.
haltA :: MachineValue a => Semantics Applicative MachineKey a ()
haltA read write = Just $
    write (F Halted) (pure 1)

-- | Load a value from a memory location to a register.
--   Functor.
load :: MachineValue a => Register -> MemoryAddress
                       -> Semantics Functor MachineKey a ()
load reg addr read write = Just $
    write (Reg reg) (read (Addr addr))

-- | Set a register value.
--   Functor.
setF :: MachineValue a => Register -> SImm8
                       -> Semantics Functor MachineKey a ()
setF reg simm read write = Just $
    write (Reg reg) ((const . unsafeFromSImm8 $ simm) <$> (read (Reg reg)))

-- | Set a register value.
--   Applicative.
setA :: MachineValue a => Register -> SImm8
                       -> Semantics Applicative MachineKey a ()
setA reg simm read write = Just $
    write (Reg reg) (pure . unsafeFromSImm8 $ simm)

-- | Store a value from a register to a memory location.
--   Functor.
store :: MachineValue a => Register -> MemoryAddress
                        -> Semantics Functor MachineKey a ()
store reg addr read write = Just $
    write (Addr addr) (read (Reg reg) )

-- | Add a value from memory location to one in a register.
--   Applicative.
add :: MachineValue a => Register -> MemoryAddress
                      -> Semantics Applicative MachineKey a ()
add reg addr = \read write -> Just $
    let result = (+) <$> read (Reg reg) <*> read (Addr addr)
    in  write (F Zero)  result *>
        write (Reg reg) result


-- | Add a value from memory location to one in a register.
--   Applicative.
addV :: MachineValue v => Register -> MemoryAddress
                       -> SemanticsV Applicative MachineKey v ()
addV reg addr = \read write -> Just $
    let void :: a -> ()
        void _ = ()

        -- write1 :: MachineValue v => MachineKey -> f v -> f ()
        write1 k v = void <$> write k v

        -- write2 :: MachineValue v => MachineKey -> MachineKey -> f v -> f ()
        -- write2 k1 k2 v = write1 k1 (write k2 v)
        write2 k1 k2 v = void <$> write k1 (write k2 v)

        result = (+) <$> read (Reg reg) <*> read (Addr addr)
    in  write2 (F Zero) (Reg reg) result

-- | Add a value from memory location to one in a register. Tracks overflow.
--   Selective.
addS :: MachineValue a => Register -> MemoryAddress -> Semantics Selective MachineKey a ()
addS reg addr = \read write -> Just $
    let arg1   = read (Reg reg)
        arg2   = read (Addr addr)
        result = (+) <$> read (Reg reg) <*> read (Addr addr)
        o = willOverflowPure <$> arg1 <*> arg2
        -- o1 = gt <$> arg2 <*> pure 0
        -- o2 = gt <$> arg1 <*> ((-) <$> pure maxBound <*> arg2)
        -- o3 = lt <$> arg2 <*> pure 0
        -- o4 = lt <$> arg1 <*> ((-) <$> pure minBound <*> arg2)
        -- o  = or <$> (and <$> o1 <*> o2)
        --         <*> (and <$> o3 <*> o4)
    in  write (F Overflow) o *>
        write (Reg reg) result *>
        write (F Zero)  result

-- | A pure check for integer overflow during addition.
willOverflowPure :: MachineValue a => a -> a -> a
willOverflowPure x y =
    let o1 = gt y 0
        o2 = gt x((-) maxBound y)
        o3 = lt y 0
        o4 = lt x((-) minBound y)
    in  or (and o1 o2)
           (and o3 o4)


-- | Sub a value from memory location to one in a register.
--   Applicative.
sub :: MachineValue a => Register -> MemoryAddress
                      -> Semantics Applicative MachineKey a ()
sub reg addr = \read write -> Just $
    let result = (-) <$> read (Reg reg) <*> read (Addr addr)
    in  write (F Zero)  result *>
        write (Reg reg) result

-- | Multiply a value from memory location to one in a register.
--   Applicative.
mul :: MachineValue a => Register -> MemoryAddress
                      -> Semantics Applicative MachineKey a ()
mul reg addr = \read write -> Just $
    let result = (*) <$> read (Reg reg) <*> read (Addr addr)
    in  write (F Zero)  result *>
        write (Reg reg) result

-- | Subtract a value from memory location to one in a register.
--   Applicative.
div :: MachineValue a => Register -> MemoryAddress
                      -> Semantics Applicative MachineKey a ()
div reg addr = \read write -> Just $
    let result = Value.div <$> read (Reg reg) <*> read (Addr addr)
    in  write (F Zero)  result *>
        write (Reg reg) result

mod :: MachineValue a => Register -> MemoryAddress
                      -> Semantics Applicative MachineKey a ()
mod reg addr = \read write -> Just $
    let result = Value.mod <$> read (Reg reg) <*> read (Addr addr)
    in  write (F Zero)  result *>
        write (Reg reg) result

abs :: MachineValue a => Register -> Semantics Functor MachineKey a ()
abs reg = \read write -> Just $
    let result = Prelude.abs <$> read (Reg reg)
    in  write (Reg reg) result

-- | Unconditional jump.
--   Functor.
jump :: MachineValue a => SImm8 -> Semantics Functor MachineKey a ()
jump simm read write = Just $
    write IC (fmap ((+) . unsafeFromSImm8 $ simm) (read IC))

-- | Indirect memory access.
--   Monadic.
-- loadMI :: MachineValue a => Register -> MemoryAddress
--                          -> Semantics Monad MachineKey a ()
-- loadMI = undefined
    -- Just $ do
    -- addr' <- read (Addr addr)
    -- write (Reg reg) (read (Addr addr'))

-- | Jump if 'Zero' flag is set.
--   Selective.
jumpZero :: MachineValue a => SImm8 -> Semantics Selective MachineKey a ()
jumpZero simm read write = Just $
    whenS (unsafeToBool <$> (eq <$> read (F Zero) <*> pure 0))
          (write IC (fmap ((+) . unsafeFromSImm8 $ simm) (read IC)))
--------------------------------------------------------------------------------
executeInstruction :: Semantics Monad MachineKey Value ()
executeInstruction = \read write -> Just $ do
    -- fetch instruction
    ic <- read IC
    write IR (read (Prog ic))
    -- increment instruction counter
    write IC (pure $ ic + 1)
    -- read instruction register and execute the instruction
    i <- read IR
    fromJust $ semanticsM (decode i) read write
