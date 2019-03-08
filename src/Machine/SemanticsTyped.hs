{-# LANGUAGE ConstraintKinds, RankNTypes,
             ScopedTypeVariables,
             FlexibleContexts,
             FlexibleInstances,
             TypeApplications,
             TypeFamilies,
             GADTs,
             StandaloneDeriving,
             MultiWayIf,
             LambdaCase, IncoherentInstances #-}

module Machine.SemanticsTyped where

import Data.Functor (void)
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
data MachineKey a where
    Reg  :: Register -> MachineKey Value
    -- ^ register
    Addr :: MemoryAddress -> MachineKey Value
    -- ^ memory address
    F    :: Flag -> MachineKey Bool
    -- ^ flag
    IC   :: MachineKey InstructionAddress
    -- ^ instruction counter
    IR   :: MachineKey InstructionCode
    -- ^ instruction register
    Prog :: InstructionAddress -> MachineKey InstructionCode
    -- ^ program memory address

deriving instance Show a => Show (MachineKey a)

-- | We amend the standard 'Monad' constraint to include 'Selective' into
--   the hierarchy
type Monad m = (Selective m, Prelude.Monad m)

-- | Functorial semantics is data independent and may have a most one
--   static dependency. May be used for
--   static analysis.
semanticsF :: Instruction -> FS Functor MachineKey ()
semanticsF = \case
    -- Halt           -> haltF
    Load reg addr  -> load reg addr
    -- LoadMI _ _     -> const (const Nothing)
    Set reg simm8  -> setF reg simm8
    Store reg addr -> store reg addr
    -- Add _ _        -> const (const Nothing)
    -- Sub _ _        -> const (const Nothing)
    -- Mul _ _        -> const (const Nothing)
    -- Div _ _        -> const (const Nothing)
    -- Mod _ _        -> const (const Nothing)
    Abs reg        -> abs reg
    Jump simm8     -> jump simm8
    -- JumpZero simm8 -> const (const Nothing)

-- | Applicative semantics is data independent. May be used for
--   static code analysis.
--
--   Note: applicative semantics cannot interact with flags.
semanticsA :: Instruction -> FS Applicative MachineKey ()
semanticsA i = case i of
    Halt           -> haltA
    Load reg addr  -> load reg addr
    -- LoadMI _ _     -> const (const Nothing)
    Set reg simm8  -> setF reg simm8
    Store reg addr -> store reg addr
    Add reg addr   -> add reg addr
    Sub reg addr   -> sub reg addr
    Mul reg addr   -> mul reg addr
    Div reg addr   -> div reg addr
    Mod reg addr   -> mod reg addr
    Abs reg        -> abs reg
    Jump simm8     -> jump simm8
    -- JumpZero simm8 -> undefined

semanticsS :: Instruction -> FS Selective MachineKey ()
semanticsS i = case i of
    Halt           -> semanticsA i
    Load   _ _     -> semanticsA i
    -- LoadMI _ _     -> const (const Nothing)
    Set reg simm8  -> setF reg simm8
    Store  _ _     -> semanticsA i
    Add reg addr   -> add reg addr
    Sub reg addr   -> semanticsA i
    Mul reg addr   -> semanticsA i
    Div reg addr   -> semanticsA i
    Mod reg addr   -> semanticsA i
    Abs _          -> semanticsA i
    Jump _         -> semanticsA i
    -- JumpZero simm8 -> jumpZero simm8

-- | Monadic semantics may involve data dynamic analysis and must be executed
--   on a concrete machine state.
--
--   Note: Indirect memory access ('LoadMI') and conditional jump ('JumpZero')
--   instruction may be only assigned monadic semantics.
semanticsM :: Instruction -> FS Monad MachineKey ()
semanticsM i = case i of
    Halt            -> semanticsS i
    Load   _ _      -> semanticsS i
    -- LoadMI reg addr -> undefined -- loadMI reg addr
    Set reg simm8   -> setF reg simm8
    Store  _ _      -> semanticsS i
    Add reg addr    -> add reg addr
    Sub reg addr    -> semanticsS i
    Mul reg addr    -> semanticsS i
    Div reg addr    -> semanticsS i
    Mod reg addr    -> semanticsS i
    Abs _           -> semanticsS i
    Jump _          -> semanticsS i
    -- JumpZero simm8  -> jumpZero simm8

-- | Halt the execution.
--   Functor.
-- haltF :: FS Functor MachineKey Bool
-- haltF read write =
--     write (F Halted) ((const (point True)) <$> read (F Halted))

-- | Halt the execution.
--   Applicative.
haltA :: FS Applicative MachineKey ()
haltA read write = void <$>
    write (F Halted) (read (F Halted)) -- (pure True)

instance Num a => Num (g a) where
instance Bounded a => Bounded (g a) where
instance MachineValue a => MachineValue (g a) where
instance Num Bool where
instance MachineValue Bool where

-- | Load a value from a memory location to a register.
--   Functor.
load :: Register -> MemoryAddress -> FS Functor MachineKey ()
load reg addr read write = void <$>
    write (Reg reg) (read (Addr addr))

-- | Set a register value.
--   Functor.
setF :: Register -> SImm8 -> FS Functor MachineKey ()
setF reg simm read write = void <$>
    write (Reg reg) ((const . unsafeFromSImm8 $ simm) <$> (read (Reg reg)))

-- -- | Set a register value.
-- --   Applicative.
-- setA :: Register -> SImm8
--                        -> FS Applicative MachineKey ()
-- setA reg simm read write = void $
--     write (Reg reg) (pure . unsafeFromSImm8 $ simm)

-- | Store a value from a register to a memory location.
--   Functor.
store :: Register -> MemoryAddress
                        -> FS Functor MachineKey ()
store reg addr read write = void <$>
    write (Addr addr) (read (Reg reg) )

-- | Add a value from memory location to one in a register.
--   Applicative.
add :: Register -> MemoryAddress -> FS Applicative MachineKey ()
add reg addr = \read write -> void <$>
    let x = read (Reg reg)
        y = read (Addr addr)
        result = (+) <$> x <*> y
    in  write (F Zero)  (((== 0) <$>) <$> result) *>
        write (Reg reg) result

-- -- -- | Add a value from memory location to one in a register.
-- -- --   Applicative.
-- -- addV :: MachineValue v => Register -> MemoryAddress
-- --                        -> SemanticsV Applicative MachineKey v ()
-- -- addV reg addr = \read write -> void $
-- --     let void :: a -> ()
-- --         void _ = ()

-- --         -- write1 :: MachineValue v => MachineKey -> f v -> f ()
-- --         write1 k v = void <$> write k v

-- --         -- write2 :: MachineValue v => MachineKey -> MachineKey -> f v -> f ()
-- --         -- write2 k1 k2 v = write1 k1 (write k2 v)
-- --         write2 k1 k2 v = void <$> write k1 (write k2 v)

-- --         result = (+) <$> read (Reg reg) <*> read (Addr addr)
-- --     in  write2 (F Zero) (Reg reg) result

-- -- -- | Add a value from memory location to one in a register. Tracks overflow.
-- -- --   Selective.
-- -- addS :: MachineValue a => Register -> MemoryAddress -> FS Selective MachineKey ()
-- -- addS reg addr = \read write -> void $
-- --     let arg1   = read (Reg reg)
-- --         arg2   = read (Addr addr)
-- --         result = (+) <$> read (Reg reg) <*> read (Addr addr)
-- --         o = willOverflowPure <$> arg1 <*> arg2
-- --         -- o1 = gt <$> arg2 <*> pure 0
-- --         -- o2 = gt <$> arg1 <*> ((-) <$> pure maxBound <*> arg2)
-- --         -- o3 = lt <$> arg2 <*> pure 0
-- --         -- o4 = lt <$> arg1 <*> ((-) <$> pure minBound <*> arg2)
-- --         -- o  = or <$> (and <$> o1 <*> o2)
-- --         --         <*> (and <$> o3 <*> o4)
-- --     in  write (F Overflow) o *>
-- --         write (Reg reg) result *>
-- --         write (F Zero)  result

-- -- -- | A pure check for integer overflow during addition.
-- -- willOverflowPure :: MachineValue a => a -> a -> a
-- -- willOverflowPure x y =
-- --     let o1 = gt y 0
-- --         o2 = gt x((-) maxBound y)
-- --         o3 = lt y 0
-- --         o4 = lt x((-) minBound y)
-- --     in  or (and o1 o2)
-- --            (and o3 o4)


-- | Sub a value from memory location to one in a register.
--   Applicative.
sub :: Register -> MemoryAddress -> FS Applicative MachineKey ()
sub reg addr = \read write -> void <$>
    let result = (-) <$> read (Reg reg) <*> read (Addr addr)
    in write (F Zero)  (((== 0) <$>) <$> result) *>
        write (Reg reg) result

-- | Multiply a value from memory location to one in a register.
--   Applicative.
mul :: Register -> MemoryAddress
                      -> FS Applicative MachineKey ()
mul reg addr = \read write -> void <$>
    let result = (*) <$> read (Reg reg) <*> read (Addr addr)
    in write (F Zero)  (((== 0) <$>) <$> result) *>
        write (Reg reg) result

-- | Subtract a value from memory location to one in a register.
--   Applicative.
div :: Register -> MemoryAddress
                      -> FS Applicative MachineKey ()
div reg addr = \read write -> void <$>
    let result = Value.div <$> read (Reg reg) <*> read (Addr addr)
    in write (F Zero)  (((== 0) <$>) <$> result) *>
        write (Reg reg) result

mod :: Register -> MemoryAddress
                      -> FS Applicative MachineKey ()
mod reg addr = \read write -> void <$>
    let result = Value.mod <$> read (Reg reg) <*> read (Addr addr)
    in write (F Zero)  (((== 0) <$>) <$> result) *>
        write (Reg reg) result

abs :: Register -> FS Functor MachineKey ()
abs reg = \read write -> void <$>
    let result = Prelude.abs <$> read (Reg reg)
    in  write (Reg reg) result

-- | Unconditional jump.
--   Functor.
jump :: SImm8 -> FS Functor MachineKey ()
jump simm read write = void <$>
    write IC (fmap ((+) . unsafeFromSImm8 $ simm) (read IC))

-- -- -- | Indirect memory access.
-- -- --   Monadic.
-- -- -- loadMI :: MachineValue a => Register -> MemoryAddress
-- -- --                          -> Semantics Monad MachineKey ()
-- -- -- loadMI = undefined
-- --     -- void <$> do
-- --     -- addr' <- read (Addr addr)
-- --     -- write (Reg reg) (read (Addr addr'))

-- -- | Jump if 'Zero' flag is set.
-- --   Selective.
-- jumpZero :: SImm8 -> FS Selective MachineKey ()
-- jumpZero simm read write =
--     whenS (read (F Zero))
--           (write IC (fmap ((+) . unsafeFromSImm8 $ simm) (read IC)))
-- -- -- -- --------------------------------------------------------------------------------
-- -- -- executeInstruction :: Semantics Monad MachineKey Value ()
-- -- -- executeInstruction = \read write -> Just $ do
-- -- --     -- fetch instruction
-- -- --     ic <- read IC
-- -- --     write IR (read (Prog ic))
-- -- --     -- increment instruction counter
-- -- --     write IC (pure $ ic + 1)
-- -- --     -- read instruction register and execute the instruction
-- -- --     i <- read IR
-- -- --     fromJust $ semanticsM (decode i) read write
