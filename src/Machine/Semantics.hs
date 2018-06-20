{-# LANGUAGE ConstraintKinds, RankNTypes,
             ScopedTypeVariables,
             FlexibleContexts,
             FlexibleInstances #-}

module Machine.Semantics where

import Prelude hiding (Monad, read)
import qualified Prelude (Monad)
import Control.Monad (join)
import Metalanguage
import Machine.Types
import Machine.Instruction
import Data.List.NonEmpty
import Control.Selective

-- | 'MachineKey' will instantiate the 'k' type variable in the 'Semantics'
--   metalanguage.
data MachineKey = Reg  Register
                | Addr MemoryAddress
                | F    Flag
                | IC
                | IR
                | Prog InstructionAddress
    deriving (Show, Eq, Ord)

type Monad m = (Selective m, Prelude.Monad m)

-- | Functorial semantics is data independent and may have a most one
--   static dependency. May be used for
--   static code analysis.
--
--   Note: applicative semantics cannot interact with flags.
semanticsF :: Instruction -> Semantics Functor MachineKey Value ()
semanticsF Halt              = haltF
semanticsF (Load reg addr)   = load reg addr
semanticsF (LoadMI _ _)      = const (const Nothing)
semanticsF (Set reg simm)    = setF reg simm
semanticsF (Store reg addr)  = store reg addr
semanticsF (Add reg addr)    = const (const Nothing)
semanticsF (Jump simm)       = jump simm
semanticsF (JumpZero _)      = const (const Nothing)

-- | Applicative semantics is data independent. May be used for
--   static code analysis.
--
--   Note: applicative semantics cannot interact with flags.
semanticsA :: Instruction -> Semantics Applicative MachineKey Value ()
semanticsA Halt              = haltA
semanticsA (Load reg addr)   = load reg addr
semanticsA (LoadMI _ _)      = const (const Nothing)
semanticsA (Set reg simm)    = setA reg simm
semanticsA (Store reg addr)  = store reg addr
semanticsA (Add reg addr)    = add reg addr
semanticsA (Jump simm)       = jump simm
semanticsA (AdjustVelocity reg addr)  = adjust reg addr
semanticsA (CheckOperationStatus reg addr1 addr2) = statusCheck reg addr1 addr2

semanticsS :: Instruction -> Semantics Selective MachineKey Value ()
semanticsS (JumpZero simm)   = jumpZero simm
semanticsS (LoadMI _ _)      = const (const Nothing)
semantics i                  = semanticsA i

-- | Monadic semantics may involve data dynamic analysis and must be executed
--   on a concrete machine state.
--
--   Note: Indirect memory access ('LoadMI') and conditional jump ('JumpZero')
--   instruction may be only assigned monadic semantics.
semanticsM :: Instruction -> Semantics Monad MachineKey Value ()
semanticsM (LoadMI reg addr) = loadMI reg addr
semanticsM i                 = semanticsS i

-- | Halt the execution.
--   Functor.
haltF :: Semantics Functor MachineKey Value ()
haltF read write = Just $
    write (F Halted) ((const 1) <$> read (F Halted))

-- | Halt the execution.
--   Applicative.
haltA :: Semantics Applicative MachineKey Value ()
haltA read write = Just $
    write (F Halted) (pure 1)

-- | Load a value from a memory location to a register.
--   Functor.
load :: Register -> MemoryAddress -> Semantics Functor MachineKey Value ()
load reg addr read write = Just $
    write (Reg reg) (read (Addr addr))

-- | Set a register value.
--   Functor.
setF :: Register -> Byte -> Semantics Functor MachineKey Value ()
setF reg simm read write = Just $
    write (Reg reg) ((const simm) <$> (read (Reg reg)))

-- | Set a register value.
--   Applicative.
setA :: Register -> Byte -> Semantics Applicative MachineKey Value ()
setA reg simm read write = Just $
    write (Reg reg) (pure simm)

-- | Store a value from a register to a memory location.
--   Functor.
store :: Register -> MemoryAddress -> Semantics Functor MachineKey Value ()
store reg addr read write = Just $
    write (Addr addr) (read (Reg reg) )

-- | Add a value from memory location to one in a register.
--   Applicative.
add :: Register -> MemoryAddress -> Semantics Applicative MachineKey Value ()
add reg addr = \read write -> Just $
    let result = (+)    <$> read (Reg reg) <*> read (Addr addr)
        isZero = (== 0) <$> result
    in  write (Reg reg) result *>
        write (F Zero)  (boolToValue <$> isZero)

boolToValue False = 0
boolToValue True  = 1

adjust :: Register -> MemoryAddress -> Semantics Applicative MachineKey Value ()
adjust reg addr = \read write -> Just $
    let result = read (Addr addr) *> read (Reg reg)
    in  write (Addr addr) result

statusCheck :: Register -> MemoryAddress -> MemoryAddress
            -> Semantics Applicative MachineKey Value ()
statusCheck reg addr1 addr2 read write = Just $
    let result = read (Addr addr1) *> read (Addr addr2)
    in  write (Reg reg) result

-- | Unconditional jump.
--   Functor.
jump :: Byte -> Semantics Functor MachineKey Value ()
jump simm read write = Just $
    write IC (fmap (+ simm) (read IC))

-- | Indirect memory access.
--   Monadic.
loadMI :: Register -> MemoryAddress -> Semantics Monad MachineKey Value ()
loadMI reg addr read write = Just $ do
    addr' <- read (Addr addr)
    write (Reg reg) (read (Addr addr'))

-- | Jump if 'Zero' flag is set.
--   Selective.
jumpZero :: Byte -> Semantics Selective MachineKey Value ()
jumpZero simm read write = Just $
    ifS ((==) <$> read (F Zero) <*> pure 1)
        (write IC ((+) <$> read IC <*> pure simm))
        (pure ())

blockSemanticsA :: [Instruction] -> Semantics Applicative MachineKey Value ()
blockSemanticsA xs = \read write->
    foldr (\x acc -> ((*>)) <$> acc <*> semanticsA x read write) nop xs
    where nop = Just $ pure ()

blockSemanticsS :: [Instruction] -> Semantics Selective MachineKey Value ()
blockSemanticsS xs = \read write->
    foldr (\x acc -> ((*>)) <$> acc <*> semanticsS x read write) nop xs
    where nop = Just $ pure ()