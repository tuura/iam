{-# LANGUAGE ConstraintKinds, RankNTypes,
             ScopedTypeVariables,
             FlexibleContexts,
             FlexibleInstances,
             TypeApplications,
             TypeFamilies,
             MultiWayIf #-}

module Machine.Semantics where

import Prelude hiding (Monad, read, div, abs, and, or)
import qualified Prelude (Monad, div, abs)
import Data.Maybe (fromJust)
import Control.Monad (join)
import Data.SBV (Boolean (..))
import Metalanguage
import Machine.Types
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
semanticsF :: InstructionCode -> Semantics Functor MachineKey Value ()
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
semanticsA :: InstructionCode -> Semantics Applicative MachineKey Value ()
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
                abs (decodeRegister . extractRegister $ expandedCode)

semanticsS :: InstructionCode -> Semantics Selective MachineKey Value ()
semanticsS code =
    let expandedCode = blastLE code
        opcode = take 6 expandedCode
    in if | opcode == [false, false, false, true, true, true]    ->
                jumpZero (fromBitsLE $ extractByteJump expandedCode)
          | opcode == [false, false, false, true, false, true]   ->
                addS (decodeRegister . extractRegister $ expandedCode)
                     (fromBitsLE $ extractMemoryAddress expandedCode)
          | otherwise -> semanticsA code

-- | Monadic semantics may involve data dynamic analysis and must be executed
--   on a concrete machine state.
--
--   Note: Indirect memory access ('LoadMI') and conditional jump ('JumpZero')
--   instruction may be only assigned monadic semantics.
semanticsM :: InstructionCode -> Semantics Monad MachineKey Value ()
semanticsM code =
    let expandedCode = blastLE code
        opcode = take 6 expandedCode
    in if | opcode == [false, false, false, false, true, false]   ->
                loadMI (decodeRegister . extractRegister $ expandedCode)
                       (fromBitsLE $ extractMemoryAddress expandedCode)
          | otherwise -> semanticsS code

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
    let result = (+) <$> read (Reg reg) <*> read (Addr addr)
    in  write (F Zero)  result *>
        write (Reg reg) result

-- | Add a value from memory location to one in a register. Tracks overflow.
--   Selective.
addS :: Register -> MemoryAddress -> Semantics Selective MachineKey Value ()
addS reg addr = \read write -> Just $
    let arg1   = read (Reg reg)
        arg2   = read (Addr addr)
        result = (+) <$> arg1 <*> arg2
        o1 = (>) <$> arg2 <*> pure 0
        o2 = (>) <$> arg1 <*> ((-) <$> pure maxBound <*> arg2)
        o3 = (<) <$> arg2 <*> pure 0
        o4 = (<) <$> arg1 <*> ((-) <$> pure minBound <*> arg2)
        o  = (|||) <$> ((&&&) <$> o1 <*> o2)
                   <*> ((&&&) <$> o3 <*> o4)
    in
        ifS o (write (F Overflow) (pure 1)) (pure ())
        *>
        write (Reg reg) result

-- addS :: ( Num v, Bounded v, Eq v, Enum v, Ord v
--         , Boolean (BoolType v), Eq (BoolType v)
--         , IsRegister r, IsMemoryAddress addr, IsFlag flag)
--      => r -> addr -> Semantics Selective MachineKey Value ()
-- addS reg addr = \read write -> Just $
--     bindS (read (Reg reg)) $ \arg1 ->
--         bindS (read (Addr addr)) $ \arg2 ->
--             let result = arg1 + arg2
--                 o = arg2 > 0 && arg1 > (maxBound - arg2) ||
--                     arg2 < 0 && arg1 < (minBound - arg2)
--             in if o then (write (F Overflow) (pure 1)) else (pure ()) *>
--                write (Reg reg) (pure $ result)

    -- ifS ((==) <$> read (F Zero) <*> pure 0)
    --     (write IC $ pure simm)
    --     (write IC $ read IC)

-- -- | Instruction @add rX, dmemaddr@ is implemented as @rX = rX + [dmemaddr]@.
-- add :: Register -> MemoryAddress -> Redfin ()
-- add rX dmemaddr = do
--     state <- readState
--     arg1 <- readRegister rX
--     arg2 <- readMemory dmemaddr
--     let overflow = arg2 .> 0 &&& arg1 .> (maxBound @Value - arg2) |||
--                    arg2 .< 0 &&& arg1 .< (minBound @Value - arg2)
--     let overflowState = snd $ redfin (writeFlag Overflow true) state
--     writeState $ ite overflow overflowState state
--     writeRegister rX (arg1 + arg2)

-- | Sub a value from memory location to one in a register.
--   Applicative.
sub :: Register -> MemoryAddress -> Semantics Applicative MachineKey Value ()
sub reg addr = \read write -> Just $
    let result = (-) <$> read (Reg reg) <*> read (Addr addr)
    in  write (F Zero)  result *>
        write (Reg reg) result

-- | Multiply a value from memory location to one in a register.
--   Applicative.
mul :: Register -> MemoryAddress -> Semantics Applicative MachineKey Value ()
mul reg addr = \read write -> Just $
    let result = (*) <$> read (Reg reg) <*> read (Addr addr)
    in  write (F Zero)  result *>
        write (Reg reg) result

-- | Subtract a value from memory location to one in a register.
--   Applicative.
div :: Register -> MemoryAddress -> Semantics Applicative MachineKey Value ()
div reg addr = \read write -> Just $
    let result = Prelude.div <$> read (Reg reg) <*> read (Addr addr)
    in  write (F Zero)  result *>
        write (Reg reg) result

abs :: Register -> Semantics Applicative MachineKey Value ()
abs reg = \read write -> Just $
    let result = Prelude.abs <$> read (Reg reg)
    in  write (Reg reg) result

-- | Unconditional absolute jump.
--   Functor.
jump :: Byte -> Semantics Functor MachineKey Value ()
jump simm read write = Just $
    write IC (fmap (const simm) (read IC))

-- | Indirect memory access.
--   Monadic.
loadMI :: Register -> MemoryAddress -> Semantics Monad MachineKey Value ()
loadMI reg addr read write = Just $ do
    addr' <- read (Addr addr)
    write (Reg reg) (read (Addr addr'))

-- | Jump (absolute) if 'Zero' flag is set.
--   Selective.
jumpZero :: Byte -> Semantics Selective MachineKey Value ()
jumpZero simm read write = Just $
    ifS ((==) <$> read (F Zero) <*> pure 0)
        (write IC $ pure simm)
        (write IC $ read IC)
--------------------------------------------------------------------------------
executeInstruction :: Semantics Monad MachineKey Value ()
executeInstruction = \read write -> Just $ do
    -- fetch instruction
    ic <- read IC
    write IR (read (Prog ic))
    -- increment instruction counter
    write IC ((+ 1) <$> read IC)
    -- read instruction register and execute the instruction
    i <- read IR
    fromJust $ semanticsM i read write

