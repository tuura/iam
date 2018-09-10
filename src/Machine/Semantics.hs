{-# LANGUAGE ConstraintKinds, RankNTypes,
             ScopedTypeVariables,
             FlexibleContexts,
             FlexibleInstances,
             TypeApplications,
             TypeFamilies,
             MultiWayIf #-}

module Machine.Semantics where

import Prelude hiding (Monad, read, div, mod, abs, and, or)
import qualified Prelude (Monad, div, mod, abs)
import Data.Maybe (fromJust)
import Control.Monad (join)
import Data.SBV (Boolean (..))
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
data MachineKey = Reg Register      -- register
                | Addr MemoryAddress  -- memory address
                | F    Flag  -- flag
                | IC         -- instruction counter
                | IR         -- instruction register
                | Prog InstructionAddress -- program memory address
    deriving (Show, Eq, Ord)

-- | We amend the standard 'Monad' constraint to include 'Selective' into
--   the hierarchy
type Monad m = (Selective m, Prelude.Monad m)

-- | Functorial semantics is data independent and may have a most one
--   static dependency. May be used for
--   static analysis.
semanticsF :: MachineValue a => InstructionCode
                             -> Semantics Functor MachineKey a ()
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
                     (fromBitsLE $ extractSImm8 expandedCode)
          | opcode == [false, false, false, true, false, false]   ->
                store (decodeRegister . extractRegister $ expandedCode)
                      (fromBitsLE $ extractMemoryAddress expandedCode)
          | opcode == [false, false, false, true, false, true]   ->
                const (const Nothing)
          | opcode == [false, false, false, true, true, false]   ->
                jump (fromBitsLE $ extractSImm8Jump expandedCode)
          | opcode == [false, false, false, true, true, true]    ->
                const (const Nothing)

-- | Applicative semantics is data independent. May be used for
--   static code analysis.
--
--   Note: applicative semantics cannot interact with flags.
semanticsA :: MachineValue a => InstructionCode
                             -> Semantics Applicative MachineKey a ()
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
                     (fromBitsLE $ extractSImm8 expandedCode)
          | opcode == [false, false, false, true, false, false]   ->
                store (decodeRegister . extractRegister $ expandedCode)
                      (fromBitsLE $ extractMemoryAddress expandedCode)
          | opcode == [false, false, false, true, false, true]   ->
                add (decodeRegister . extractRegister $ expandedCode)
                    (fromBitsLE $ extractMemoryAddress expandedCode)
          | opcode == [false, false, false, true, true, false]   ->
                jump (fromBitsLE $ extractSImm8Jump expandedCode)
          | opcode == [false, false, false, true, true, true]    ->
                const (const Nothing)
          | opcode == [false, false, true, false, false, false]    ->
                sub (decodeRegister . extractRegister $ expandedCode)
                    (fromBitsLE $ extractMemoryAddress expandedCode)
          | opcode == [false, false, true, false, false, true]    ->
                mul (decodeRegister . extractRegister $ expandedCode)
                    (fromBitsLE $ extractMemoryAddress expandedCode)
          | opcode == [false, false, true, false, true, false]    ->
                div (decodeRegister . extractRegister $ expandedCode)
                            (fromBitsLE $ extractMemoryAddress expandedCode)
          | opcode == [false, false, true, false, true, true]    ->
                mod (decodeRegister . extractRegister $ expandedCode)
                            (fromBitsLE $ extractMemoryAddress expandedCode)
          | opcode == [false, false, true, true, false, false]    ->
                abs (decodeRegister . extractRegister $ expandedCode)

semanticsS :: MachineValue a => InstructionCode
                             -> Semantics Selective MachineKey a ()
semanticsS code =
    let expandedCode = blastLE code
        opcode = take 6 expandedCode
    in if | opcode == [false, false, false, true, true, true]    ->
                jumpZero (fromBitsLE $ extractSImm8Jump expandedCode)
          | opcode == [false, false, false, true, false, true]   ->
                addS (decodeRegister . extractRegister $ expandedCode)
                     (fromBitsLE $ extractMemoryAddress expandedCode)
          | otherwise -> semanticsA code

-- | Monadic semantics may involve data dynamic analysis and must be executed
--   on a concrete machine state.
--
--   Note: Indirect memory access ('LoadMI') and conditional jump ('JumpZero')
--   instruction may be only assigned monadic semantics.
semanticsM :: MachineValue a => InstructionCode
                             -> Semantics Monad MachineKey a ()
semanticsM code =
    let expandedCode = blastLE code
        opcode = take 6 expandedCode
    in if | opcode == [false, false, false, false, true, false]   ->
                loadMI (decodeRegister . extractRegister $ expandedCode)
                       (fromBitsLE $ extractMemoryAddress expandedCode)
          | otherwise -> semanticsS code

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

-- | Add a value from memory location to one in a register. Tracks overflow.
--   Selective.
addS :: MachineValue a => Register -> MemoryAddress -> Semantics Selective MachineKey a ()
addS reg addr = \read write -> Just $
    let arg1   = read (Reg reg)
        arg2   = read (Addr addr)
        result = (+) <$> read (Reg reg) <*> read (Addr addr)
        o1 = gt <$> arg2 <*> pure 0
        o2 = gt <$> arg1 <*> ((-) <$> pure maxBound <*> arg2)
        o3 = lt <$> arg2 <*> pure 0
        o4 = lt <$> arg1 <*> ((-) <$> pure minBound <*> arg2)
        o  = or <$> (and <$> o1 <*> o2)
                <*> (and <$> o3 <*> o4)
    in  write (F Overflow) o *>
        write (Reg reg) result

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

abs :: MachineValue a => Register -> Semantics Applicative MachineKey a ()
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
loadMI :: MachineValue a => Register -> MemoryAddress
                         -> Semantics Monad MachineKey a ()
loadMI reg addr read write = -- undefined
    Just $ do
    addr' <- read (Addr addr)
    write (Reg reg) (read (Addr addr'))

-- | Jump if 'Zero' flag is set.
--   Selective.
jumpZero :: MachineValue a => SImm8 -> Semantics Selective MachineKey a ()
jumpZero simm read write = undefined
    -- Just $
    -- ifS ((==) <$> read (F Zero) <*> pure 0)
    --     (write IC (fmap ((+) . unsafeFromSImm8 $ simm) (read IC)))
    --     (write IC $ read IC)
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
    fromJust $ semanticsM i read write

