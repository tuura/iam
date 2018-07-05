{-# LANGUAGE ConstraintKinds, RankNTypes,
             ScopedTypeVariables,
             FlexibleContexts,
             FlexibleInstances,
             TypeApplications,
             TypeFamilies,
             MultiWayIf #-}

module Machine.Semantics where

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
-- 'r' is the register type
-- 'addr' is the memory address type
-- 'iaddr' is the instruction address type
-- 'flag' is the flag type
data MachineKey r addr iaddr flag = Reg r      -- register
                                  | Addr addr  -- memory address
                                  | F    flag  -- flag
                                  | IC         -- instruction counter
                                  | IR         -- instruction register
                                  | Prog iaddr -- program memory address
    deriving (Show, Eq, Ord)

-- | We amend the standard 'Monad' constraint to include 'Selective' into
--   the hierarchy
type Monad m = (Selective m, Prelude.Monad m)

-- | Functorial semantics is data independent and may have a most one
--   static dependency. May be used for
--   static analysis.
semanticsF :: ( IsRegister r, Eq r
              , IsMemoryAddress addr, MachineBits addr
              , Num code
              , IsByte byte
              , code ~ addr, code ~ byte
              , IsBool (BoolType addr), Eq (BoolType addr)
              , IsFlag flag
              , Num v , MachineBits v
              , (BoolType v) ~ (BoolType byte)
              )
       => code -> Semantics Functor (MachineKey r addr iaddr flag) v ()
semanticsF code =
    let expandedCode = blastLE code
        opcode = take 6 expandedCode
    in if | opcode == [false, false, false, false, false, false] -> haltF
          | opcode == [false, false, false, false, false, true]  ->
                load (decodeRegister . extractRegister $ expandedCode)
                     (fromBitsLE $ extractMemoryAddress expandedCode)
          | opcode == [false, false, false, false, true, false]   ->
                const (const Nothing)
          | opcode == [false, false, false, false, true, true]  ->
                setF (decodeRegister . extractRegister $ expandedCode)
                     (fromBitsLE $ extractByte expandedCode)
          | opcode == [false, false, false, true, false, false]   ->
                store (decodeRegister . extractRegister $ expandedCode)
                      (fromBitsLE $ extractMemoryAddress expandedCode)
          | opcode == [false, false, false, true, false, true]   ->
                const (const Nothing)
          | opcode == [false, false, false, true, true, false]   ->
                jump (fromBitsLE $ extractByteJump expandedCode)
          | opcode == [false, false, false, true, true, true]    ->
                const (const Nothing)

-- | Applicative semantics is data independent. May be used for
--   static code analysis.
--
--   Note: applicative semantics cannot interact with flags.
semanticsA :: ( IsRegister r, Eq r
              , IsMemoryAddress addr, MachineBits addr
              , Num code
              , IsByte byte
              , code ~ addr, code ~ byte
              , IsBool (BoolType addr), Eq (BoolType addr)
              , IsFlag flag
              , Num v , Eq v, MachineBits v
              , (BoolType v) ~ (BoolType byte)
              )
       => code -> Semantics Applicative (MachineKey r addr iaddr flag) v ()
semanticsA code =
    let expandedCode = blastLE code
        opcode = take 6 expandedCode
    in if | opcode == [false, false, false, false, false, false] -> haltA
          | opcode == [false, false, false, false, false, true]  ->
                load (decodeRegister . extractRegister $ expandedCode)
                     (fromBitsLE $ extractMemoryAddress expandedCode)
          | opcode == [false, false, false, false, true, false]   ->
                const (const Nothing)
          | opcode == [false, false, false, false, true, true]  ->
                setA (decodeRegister . extractRegister $ expandedCode)
                     (fromBitsLE $ extractByte expandedCode)
          | opcode == [false, false, false, true, false, false]   ->
                store (decodeRegister . extractRegister $ expandedCode)
                      (fromBitsLE $ extractMemoryAddress expandedCode)
          | opcode == [false, false, false, true, false, true]   ->
                add (decodeRegister . extractRegister $ expandedCode)
                    (fromBitsLE $ extractMemoryAddress expandedCode)
          | opcode == [false, false, false, true, true, false]   ->
                jump (fromBitsLE $ extractByteJump expandedCode)
          | opcode == [false, false, false, true, true, true]    ->
                const (const Nothing)

semanticsS :: ( IsRegister r, Eq r
              , IsMemoryAddress addr, MachineBits addr
              , Num code
              , IsByte byte
              , code ~ addr, code ~ byte
              , IsBool (BoolType addr), Eq (BoolType addr)
              , IsFlag flag
              , Num v , Eq v, MachineBits v
              , (BoolType v) ~ (BoolType byte)
              )
           => code -> Semantics Selective (MachineKey r addr iaddr flag) v ()
semanticsS code =
    let expandedCode = blastLE code
        opcode = take 6 expandedCode
    in if | opcode == [false, false, false, true, true, true]    ->
                jumpZero (fromBitsLE $ extractByteJump expandedCode)
          | otherwise -> semanticsA code

-- | Monadic semantics may involve data dynamic analysis and must be executed
--   on a concrete machine state.
--
--   Note: Indirect memory access ('LoadMI') and conditional jump ('JumpZero')
--   instruction may be only assigned monadic semantics.
semanticsM :: ( IsRegister r, Eq r
              , IsMemoryAddress addr, MachineBits addr
              , Num code
              , IsByte byte
              , code ~ addr, code ~ byte
              , IsBool (BoolType addr), Eq (BoolType addr)
              , IsFlag flag
              , Num v , Eq v, MachineBits v
              , (BoolType v) ~ (BoolType byte)
              , v ~ byte
              )
           => code -> Semantics Monad (MachineKey r addr iaddr flag) v ()
semanticsM code =
    let expandedCode = blastLE code
        opcode = take 6 expandedCode
    in if | opcode == [false, false, false, false, true, false]   ->
                loadMI (decodeRegister . extractRegister $ expandedCode)
                       (fromBitsLE $ extractMemoryAddress expandedCode)
          | otherwise -> semanticsS code

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
    in  write (F zero)  result *>
        write (Reg reg) result

-- | Unconditional absolute jump.
--   Functor.
jump :: Num v => v -> Semantics Functor (MachineKey r addr iaddr flag) v ()
jump simm read write = Just $
    write IC (fmap (const simm) (read IC))

-- | Indirect memory access.
--   Monadic.
loadMI :: (Num v, IsRegister r, IsMemoryAddress addr, v ~ addr)
       => r -> addr -> Semantics Monad (MachineKey r addr iaddr flag) v ()
loadMI reg addr read write = Just $ do
    addr' <- read (Addr addr)
    write (Reg reg) (read (Addr addr'))

-- | Jump (absolute) if 'Zero' flag is set.
--   Selective.
jumpZero :: (Num v, Eq v, IsFlag flag)
         => v -> Semantics Selective (MachineKey r addr iaddr flag) v ()
jumpZero simm read write = Just $
    ifS ((==) <$> read (F zero) <*> pure 0)
        (write IC $ pure simm)
        (write IC $ read IC)
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
    fromJust $ semanticsM i read write

