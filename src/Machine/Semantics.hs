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
import Metalanguage
import Machine.Types
import Machine.Instruction
import Machine.Instruction.Encode
import Machine.Instruction.Decode
import Control.Selective
import Machine.Value
import Data.Bifunctor

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
              , Integral v , Eq v, MachineBits v
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

semanticsS :: ( IsRegister r, Eq r
              , IsMemoryAddress addr, MachineBits addr
              , Num code
              , IsByte byte
              , code ~ addr, code ~ byte
              , IsBool (BoolType addr), Eq (BoolType addr)
              , IsFlag flag
              , Integral v , Eq v, Bounded v, MachineBits v, MachineOrd v
              , (BoolType v) ~ (BoolType byte)
              )
           => code -> Semantics Selective (MachineKey r addr iaddr flag) v ()
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
semanticsM :: ( IsRegister r, Eq r
              , IsMemoryAddress addr, MachineBits addr
              , Num code
              , IsByte byte
              , code ~ addr, code ~ byte
              , IsBool (BoolType addr), Eq (BoolType addr)
              , IsFlag flag
              , Integral v , Eq v, Bounded v, MachineBits v, MachineOrd v
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
    let result = (+) <$> read (Reg reg) <*> read (Addr addr)
    in  write (F zero)  result *>
        write (Reg reg) result

-- | Add a value from memory location to one in a register. Tracks overflow.
--   Selective.
addS :: ( Num v, Bounded v, Eq v, MachineOrd v
        , IsBool (BoolType v), Eq (BoolType v)
        , IsRegister r, IsMemoryAddress addr, IsFlag flag)
     => r -> addr -> Semantics Selective (MachineKey r addr iaddr flag) v ()
addS reg addr = \read write -> Just $
    let arg1   = read (Reg reg)
        arg2   = read (Addr addr)
        result = (+) <$> arg1 <*> arg2
        o1 = gt <$> arg2 <*> pure 0
        o2 = gt <$> arg1 <*> ((-) <$> pure maxBound <*> arg2)
        o3 = lt <$> arg2 <*> pure 0
        o4 = lt <$> arg1 <*> ((-) <$> pure minBound <*> arg2)
        o  = or <$> (and <$> o1 <*> o2)
                <*> (and <$> o3 <*> o4)
    in
        ifS' o (write (F overflow) (pure 1)) (pure ())
        -- *>
        -- write (Reg reg) result

    -- ifS ((==) <$> read (F zero) <*> pure 0)
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
sub :: (Num v, Eq v, IsRegister r, IsMemoryAddress addr, IsFlag flag)
     => r -> addr -> Semantics Applicative (MachineKey r addr iaddr flag) v ()
sub reg addr = \read write -> Just $
    let result = (-) <$> read (Reg reg) <*> read (Addr addr)
    in  write (F zero)  result *>
        write (Reg reg) result

-- | Multiply a value from memory location to one in a register.
--   Applicative.
mul :: (Num v, Eq v, IsRegister r, IsMemoryAddress addr, IsFlag flag)
     => r -> addr -> Semantics Applicative (MachineKey r addr iaddr flag) v ()
mul reg addr = \read write -> Just $
    let result = (*) <$> read (Reg reg) <*> read (Addr addr)
    in  write (F zero)  result *>
        write (Reg reg) result

-- | Subtract a value from memory location to one in a register.
--   Applicative.
div :: (Integral v, Eq v, IsRegister r, IsMemoryAddress addr, IsFlag flag)
     => r -> addr -> Semantics Applicative (MachineKey r addr iaddr flag) v ()
div reg addr = \read write -> Just $
    let result = Prelude.div <$> read (Reg reg) <*> read (Addr addr)
    in  write (F zero)  result *>
        write (Reg reg) result

abs :: (Num v, Eq v, IsRegister r, IsMemoryAddress addr, IsFlag flag)
     => r -> Semantics Applicative (MachineKey r addr iaddr flag) v ()
abs reg = \read write -> Just $
    let result = Prelude.abs <$> read (Reg reg)
    in  write (Reg reg) result

-- duplicate :: (Num v, Eq v, IsRegister r, IsMemoryAddress addr, IsFlag flag)
--           => r -> addr -> addr -> Semantics Applicative (MachineKey r addr iaddr flag) v ()
-- duplicate reg addr1 addr2 = \read write -> Just $
--     let -- c :: Applicative f => f (f (), f())
--         -- c = (\x -> (write (Addr addr1) (pure x), write (Addr addr2) (pure x))) <$> (read (Reg reg))
--         c = (\x -> (x, x)) <$> (read (Reg reg))
--     -- in write (Addr addr1) (fst <$> c) *> pure ()
--     in (bimap (write (Addr addr1) . pure) (write (Addr addr1) . pure) <$> c) *> pure ()
--     -- let x = read (Reg reg)
--     -- in write (Addr addr1) x *> write (Addr addr2) x

-- duplicateM :: (Num v, Eq v, IsRegister r, IsMemoryAddress addr, IsFlag flag)
--           => r -> addr -> addr -> Semantics Monad (MachineKey r addr iaddr flag) v ()
-- duplicateM reg addr1 addr2 = \read write -> Just $ do
--     x <- read (Reg reg)
--     write (Addr addr1) (pure x)
--     write (Addr addr2) (pure x)

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
executeInstruction :: (Integral v, Eq v, Bounded v, MachineBits v, MachineOrd v
            , IsByte v, IsBool (BoolType v)
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

