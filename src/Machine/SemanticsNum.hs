{-# LANGUAGE ConstraintKinds, RankNTypes,
             ScopedTypeVariables,
             FlexibleContexts,
             FlexibleInstances,
             TypeApplications,
             TypeFamilies #-}

module Machine.SemanticsNum where

import Prelude hiding (Monad, read)
import qualified Prelude (Monad)
import Data.Maybe (fromJust)
import Control.Monad (join)
import Metalanguage
import Machine.Types
import Machine.Instruction
import Machine.Instruction.Encode
import Machine.Instruction.Decode
-- import Data.List.NonEmpty
import Control.Selective
import Machine.Value

-- | 'MachineKey' will instantiate the 'k' type variable in the 'Semantics'
--   metalanguage.
data MachineKey r addr iaddr flag = Reg r
                            | Addr addr
                            | F    flag
                            | IC
                            | IR
                            | Prog iaddr
    deriving (Show, Eq, Ord)

type Monad m = (Selective m, Prelude.Monad m)

-- | Functorial semantics is data independent and may have a most one
--   static dependency. May be used for
--   static code analysis.
--
--   Note: applicative semantics cannot interact with flags.
semanticsF :: (Num v, Eq v, IsRegister r, IsMemoryAddress addr, IsFlag flag)
           => Instruction r addr flag v
           -> Semantics Functor (MachineKey r addr iaddr flag) v ()
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
semanticsA :: (Num v, Eq v, IsRegister r, IsMemoryAddress addr, IsFlag flag)
           => Instruction r addr flag v
           -> Semantics Applicative (MachineKey r addr iaddr flag) v ()
semanticsA Halt              = haltA
semanticsA (Load reg addr)   = load reg addr
semanticsA (LoadMI _ _)      = const (const Nothing)
semanticsA (Set reg simm)    = setA reg simm
semanticsA (Store reg addr)  = store reg addr
semanticsA (Add reg addr)    = add reg addr
semanticsA (Jump simm)       = jump simm
semanticsA (JumpZero simm)   = const (const Nothing)

semanticsS :: (Num v, Eq v, IsRegister r, IsMemoryAddress addr, IsFlag flag)
           => Instruction r addr flag v
           -> Semantics Selective (MachineKey r addr iaddr flag) v ()
semanticsS (JumpZero simm)   = jumpZero simm
semanticsS (LoadMI _ _)      = const (const Nothing)
semanticsS i                  = semanticsA i

-- | Monadic semantics may involve data dynamic analysis and must be executed
--   on a concrete machine state.
--
--   Note: Indirect memory access ('LoadMI') and conditional jump ('JumpZero')
--   instruction may be only assigned monadic semantics.
semanticsM :: (Num v, Eq v, IsRegister r, IsMemoryAddress addr, IsFlag flag, addr ~ v)
           => Instruction r addr flag v
           -> Semantics Monad (MachineKey r addr iaddr flag) v ()
semanticsM (LoadMI reg addr) = loadMI reg addr
semanticsM i                 = semanticsS i

-- | Halt the execution.
--   Functor.
haltF :: (IsFlag flag, Num v) => Semantics Functor (MachineKey r addr iaddr flag) v ()
haltF read write = Just $
    write (F halted) ((const 1) <$> read (F halted))

-- | Halt the execution.
--   Applicative.
haltA :: (IsFlag flag, Num v) =>
         Semantics Applicative (MachineKey r addr iaddr flag) v ()
haltA read write = Just $
    write (F halted) (pure 1)

-- | Load a value from a memory location to a register.
--   Functor.
load :: (Num v, IsRegister r, IsMemoryAddress addr)
     => r -> addr -> Semantics Functor (MachineKey r addr iaddr flag) v ()
load reg addr read write = Just $
    write (Reg reg) (read (Addr addr))

-- | Set a register value.
--   Functor.
setF ::  (Num v, IsRegister r, IsMemoryAddress addr)
     => r -> v -> Semantics Functor (MachineKey r addr iaddr flag) v ()
setF reg simm read write = Just $
    write (Reg reg) ((const simm) <$> (read (Reg reg)))

-- | Set a register value.
--   Applicative.
setA ::  (Num v, IsRegister r, IsMemoryAddress addr)
     => r -> v -> Semantics Applicative (MachineKey r addr iaddr flag) v ()
setA reg simm read write = Just $
    write (Reg reg) (pure simm)

-- | Store a value from a register to a memory location.
--   Functor.
store ::  (Num v, IsRegister r, IsMemoryAddress addr)
      => r -> addr -> Semantics Functor (MachineKey r addr iaddr flag) v ()
store reg addr read write = Just $
    write (Addr addr) (read (Reg reg) )

-- | Add a value from memory location to one in a register.
--   Applicative.
add :: (Num v, Eq v, IsRegister r, IsMemoryAddress addr, IsFlag flag)
    => r -> addr -> Semantics Applicative (MachineKey r addr iaddr flag) v ()
add reg addr = \read write -> Just $
    let result = (+)    <$> read (Reg reg) <*> read (Addr addr)
        isZero = (== 0) <$> result
    in  write (Reg reg) result *>
        write (F zero)  (boolToNum <$> isZero)

boolToNum False = 0
boolToNum True  = 1

-- -- adjust :: Register -> MemoryAddress -> Semantics Applicative (MachineKey r addr iaddr flag) Value ()
-- -- adjust reg addr = \read write -> Just $
-- --     let result = read (Addr addr) *> read (Reg reg)
-- --     in  write (Addr addr) result

-- -- statusCheck :: Register -> MemoryAddress -> MemoryAddress
-- --             -> Semantics Applicative (MachineKey r addr iaddr flag) Value ()
-- -- statusCheck reg addr1 addr2 read write = Just $
-- --     let result = read (Addr addr1) *> read (Addr addr2)
-- --     in  write (Reg reg) result

-- | Unconditional jump.
--   Functor.
jump :: Num v => v -> Semantics Functor (MachineKey r addr iaddr flag) v ()
jump simm read write = Just $
    write IC (fmap (const simm) (read IC)) -- (fmap (+ simm) (read IC))

-- | Indirect memory access.
--   Monadic.
loadMI :: (Num v, IsRegister r, IsMemoryAddress addr, v ~ addr)
       => r -> addr -> Semantics Monad (MachineKey r addr iaddr flag) v ()
loadMI reg addr read write = Just $ do
    addr' <- read (Addr addr)
    write (Reg reg) (read (Addr addr'))

-- | Jump if 'Zero' flag is set.
--   Selective.
jumpZero :: (Num v, Eq v, IsFlag flag)
         => v -> Semantics Selective (MachineKey r addr iaddr flag) v ()
jumpZero simm read write = Just $
    ifS ((==) <$> read (F zero) <*> pure 1)
        (write IC ((+) <$> read IC <*> pure simm))
        (pure ())

blockSemanticsA :: (Num v, Eq v, IsRegister r, IsMemoryAddress addr, IsFlag flag)
                => [Instruction r addr flag v]
                -> Semantics Applicative (MachineKey r addr iaddr flag) v ()
blockSemanticsA xs = \read write->
    foldr (\x acc -> ((*>)) <$> acc <*> semanticsA x read write) nop xs
    where nop = Just $ pure ()

blockSemanticsS :: (Num v, Eq v, IsRegister r, IsMemoryAddress addr, IsFlag flag)
                => [Instruction r addr flag v]
                -> Semantics Selective (MachineKey r addr iaddr flag) v ()
blockSemanticsS xs = \read write->
    foldr (\x acc -> ((*>)) <$> acc <*> semanticsS x read write) nop xs
    where nop = Just $ pure ()

blockSemanticsM :: (Num v, Eq v, IsRegister r, IsMemoryAddress addr, IsFlag flag, v ~ addr)
                => [Instruction r addr flag v]
                -> Semantics Monad (MachineKey r addr iaddr flag) v ()
blockSemanticsM xs = \read write->
    foldr (\x acc -> ((>>)) <$> acc <*> semanticsM x read write) nop xs
    where nop = Just $ pure ()

--------------------------------------------------------------------------------

executeInstruction :: (Num v, Eq v, MachineBits v, IsByte v, IsBool (BoolType v)
            , Eq (BoolType v)
            , IsInstructionCode code
            , IsRegister r, Eq r
            , IsMemoryAddress addr
            , IsInstructionAddress iaddr
            , IsFlag flag, addr ~ v, iaddr ~ v, code ~ v)
         => Semantics Monad (MachineKey r addr iaddr flag) v ()
executeInstruction = \read write -> Just $ do
    -- fetch instruction
    ic <- read IC
    write IR (read (Prog ic))
    -- increment instruction counter
    write IC ((+ 1) <$> read IC)
    -- read instruction register and execute the instruction
    i <- read IR
    fromJust $ semanticsM (decode i) read write

