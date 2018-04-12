{-# LANGUAGE ConstraintKinds, RankNTypes,
             ScopedTypeVariables,
             FlexibleContexts,
             FlexibleInstances #-}

module Machine.Semantics where

import Prelude hiding (read)
import Metalanguage
import Machine.Types
import Machine.Instruction

-- | 'MachineKey' will instantiate the 'k' type variable in the 'Semantics'
--   metalanguage.
data MachineKey = Reg  Register
         | Addr MemoryAddress
         | F    Flag
         | IC
         | IR
         | Prog InstructionAddress
    deriving (Show, Eq, Ord)

-- | Applicative semantics is data independent. May be used for
--   static code analysis.
--
--   Note: applicative semantics cannot interact with flags.
semanticsA :: Instruction -> Semantics Applicative MachineKey Value ()
semanticsA Halt              = halt
semanticsA (Load reg addr)   = load reg addr
semanticsA (LoadMI _ _)      = const (const Nothing)
semanticsA (Set reg simm)    = set reg simm
semanticsA (Store reg addr)  = store reg addr
semanticsA (Add reg addr)    = add reg addr
semanticsA (Jump simm)       = jump simm
semanticsA (JumpZero _)      = const (const Nothing)

-- | Monadic semantics may involve data dynamic analysis and must be executed
--   on a concrete machine state.
--
--   Note: Indirect memory access ('LoadMI') and conditional jump ('JumpZero')
--   instruction may be only assigned monadic semantics.
semanticsM :: Instruction -> Semantics Monad MachineKey Value ()
semanticsM (LoadMI reg addr) = loadMI reg addr
semanticsM (JumpZero simm)   = jumpZero simm
semanticsM i                 = semanticsA i

-- | Halt the execution.
--   Applicative.
halt :: Semantics Applicative MachineKey Value ()
halt _ write = Just $
    write (F Halted) (pure 1)

-- | Load a value from a memory location to a register.
--   Applicative.
load :: Register -> MemoryAddress -> Semantics Applicative MachineKey Value ()
load reg addr read write = Just $
    write (Reg reg) (read (Addr addr))

-- | Set a register value.
--   Applicative.
set :: Register -> SImm8 -> Semantics Applicative MachineKey Value ()
set reg simm _ write = Just $
    write (Reg reg) (pure simm)

-- | Store a value from a register to a memory location.
--   Applicative.
store :: Register -> MemoryAddress -> Semantics Applicative MachineKey Value ()
store reg addr read write = Just $
    write (Addr addr) (read (Reg reg) )

-- | Add a value from memory location to one in a register.
--   Applicative.
add :: Register -> MemoryAddress -> Semantics Applicative MachineKey Value ()
add reg addr read write = Just $
    let z = (+) <$> read (Reg reg) <*> read (Addr addr)
    in write (Reg reg) z *> write (F Zero) (boolToValue <$> (== 0) <$> z)
        where boolToValue False = 0
              boolToValue True  = 1

-- | Unconditional jump.
--   Applicative.
jump :: SImm8 -> Semantics Applicative MachineKey Value ()
jump simm read write = Just $
    write IC ((+) <$> read IC <*> pure simm)

-- | Indirect memory access.
--   Monadic.
loadMI :: Register -> MemoryAddress -> Semantics Monad MachineKey Value ()
loadMI reg addr read write = Just $ do
    addr' <- read (Addr addr)
    v <- read (Addr addr')
    write (Reg reg) (pure v)

-- | Jump if 'Zero' flag is set.
--   Monadic.
jumpZero :: SImm8 -> Semantics Monad MachineKey Value ()
jumpZero simm read write = Just $ do
    zero <- read (F Zero)
    if (zero == 1) then
        write IC ((+) <$> read IC <*> pure simm)
    else pure ()
