{-# LANGUAGE ConstraintKinds, RankNTypes,
             ScopedTypeVariables,
             FlexibleContexts,
             FlexibleInstances,
             DeriveDataTypeable,
             DeriveAnyClass,
             DeriveGeneric,
             StandaloneDeriving #-}
module Machine.Semantics.Symbolic where

import Prelude hiding (Monad, read)
import qualified Prelude (Monad)
import qualified GHC.Generics as GHC
import qualified Data.Data as Data
import Control.Monad.State hiding (Monad)
import Metalanguage
import Control.Selective
import Data.SBV
import Data.Maybe (fromJust)
import Unsafe.Coerce

--------------------------------------------------------------------------------
---------- Basic Types ---------------------------------------------------------
--------------------------------------------------------------------------------

type Value = Int64

type SImm8 = Int64

data Register = R0 | R1 | R2 | R3
    deriving (Show, Read, Eq, Ord, Enum, Data.Data, HasKind, SymWord)

-- | The register bank is represented by a map from registers to their values.
type RegisterBank = SFunArray Register Int64

type MemoryAddress = Int64

-- deriving instance Data.Data MemoryAddress

-- | The memory is represented by a map from memory addresses to their values.
type Memory = SFunArray Int64 Value

-- | Boolean 'Flag's indicate the current status of Iam.
data Flag = Zero
          | Halted
          deriving (Enum, Eq, Ord, Show, Read, Data.Data, HasKind, SymWord)

type FlagId = Int64

-- flagId :: Flag -> FlagId
-- flagId = fromIntegral . fromEnum

-- | The state of flags is represented by a map from flags to their values.
type Flags = SFunArray Flag Value

-- | 'Clock' is the current time measured in clock cycles. It used to model the
-- effect of the 'Iam.Semantics.wait' instruction.
type Clock = Int64

--------------------------------------------------------------------------------
---------- Instructions --------------------------------------------------------
--------------------------------------------------------------------------------

-- | Iam instructions
data Instruction = Halt
                 | Load     Register MemoryAddress
                 | LoadMI   Register MemoryAddress
                 | Set      Register SImm8
                 | Store    Register MemoryAddress
                 | Add      Register MemoryAddress
                 | Jump     SImm8
                 | JumpZero SImm8
    deriving (Eq, Ord, Show, Read, Data.Data, HasKind, SymWord)

-- | Programs are stored in program memory.
type InstructionAddress = Value

-- | The program is represented by a map from instruction addresses to codes.
type Program = SFunArray InstructionAddress Instruction

--------------------------------------------------------------------------------
---------- State ---------------------------------------------------------------
--------------------------------------------------------------------------------

data MachineKey = Reg  Register
                | Addr MemoryAddress
                | F    Flag
                | IC
                | IR
                | Prog InstructionAddress
    deriving (Show, Eq, Ord, Read, Data.Data, HasKind, SymWord)

-- | The state of a Iam machine
data MachineState = MachineState
    { registers           :: RegisterBank
    , instructionCounter  :: SBV InstructionAddress
    , instructionRegister :: SBV Instruction
    , flags               :: Flags
    , memory              :: Memory
    , program             :: Program
    , clock               :: SBV Clock
    } deriving (Show, GHC.Generic, Mergeable)

readState :: MachineKey -> State MachineState (SBV Value)
readState k = do
    s <- get
    case k of Reg  reg  -> pure $ readArray (registers s) (literal reg)
              Addr addr -> pure $ readArray (memory s)    (literal addr)

writeState :: MachineKey -> SBV Value -> State MachineState ()
writeState k v = do
    s <- get
    case k of Reg reg   -> let rs' = writeArray (registers s) (literal reg) v
                           in put $ s {registers = rs'}
              Addr addr -> let mem' = writeArray (memory s) (literal addr) v
                           in put $ s {memory = mem'}

simulate :: Int -> MachineState -> MachineState
simulate steps state
    | steps <= 0 = state
    | otherwise  = ite halted state (simulate (steps - 1) nextState)
  where
    halted    = readArray (flags state) (literal Halted) .== literal 0
    nextState = snd $ undefined -- runState executeInstruction state

-- | Advance the clock by a given number of clock cycles.
delay :: Clock -> State MachineState ()
delay cycles = modify $ \(MachineState rs ic ir fs m p  c         )
                      -> MachineState rs ic ir fs m p (c + literal cycles)

-- | Lookup the 'InstructionCode' at the given 'InstructionAddress'. If the
-- program has no code associated with the address, the function returns 0 and
-- raises the 'OutOfProgram' error flag. We assume that it takes 1 clock cycle
-- to access the program memory in hardware.
readProgram :: SBV InstructionAddress -> State MachineState (SBV Instruction)
readProgram address = do
    state <- get
    delay 1
    return $ readArray (program state) address

-- | Fetch the instruction code pointed to by the instruction counter and store
-- it in the instruction register. We assume that instruction fetch takes one
-- clock cycle.
fetchInstruction :: State MachineState ()
fetchInstruction = do
    state <- get
    writeInstructionRegister =<< readProgram (instructionCounter state)

-- | Increment the instruction counter.
incrementInstructionCounter :: State MachineState ()
incrementInstructionCounter = modify $ \(MachineState rs  ic      ir fs m p c)
                                      -> MachineState rs (ic + literal 1) ir fs m p c

-- | Read the instruction register.
readInstructionRegister :: State MachineState (SBV Instruction)
readInstructionRegister = instructionRegister <$> get

-- | Write a given 'InstructionCode' to the instruction register.
writeInstructionRegister :: SBV Instruction -> State MachineState ()
writeInstructionRegister instruction =
    modify $ \(MachineState rs ic _           fs m p c)
            -> MachineState rs ic instruction fs m p c

-- executeInstruction :: State MachineState ()
-- executeInstruction = do
--     fetchInstruction
--     incrementInstructionCounter
--     _abc =<< readInstructionRegister

-- monadic :: Instruction -> State MachineState ()
-- monadic i = fromJust $ semanticsM i readState writeState
--------------------------------------------------------------------------------
---------- Semantics -----------------------------------------------------------
--------------------------------------------------------------------------------

type Monad m = (Selective m, Prelude.Monad m)

-- | Functorial semantics is data independent and may have a most one
--   static dependency. May be used for
--   static code analysis.
--
-- --   Note: applicative semantics cannot interact with flags.
-- semanticsF :: Instruction -> Semantics Applicative (SBV MachineKey) (SBV Value) ()
-- semanticsF Halt              = haltF
-- semanticsF (Load reg addr)   = load reg addr
-- semanticsF (LoadMI _ _)      = const (const Nothing)
-- semanticsF (Set reg simm)    = setF reg simm
-- semanticsF (Store reg addr)  = store reg addr
-- semanticsF (Add reg addr)    = const (const Nothing)
-- semanticsF (Jump simm)       = jump simm
-- semanticsF (JumpZero _)      = const (const Nothing)

-- -- | Applicative semantics is data independent. May be used for
-- --   static code analysis.
-- --
-- --   Note: applicative semantics cannot interact with flags.
-- semanticsA :: Instruction -> Semantics Applicative MachineKey Value ()
-- semanticsA Halt              = haltA
-- semanticsA (Load reg addr)   = load reg addr
-- semanticsA (LoadMI _ _)      = const (const Nothing)
-- semanticsA (Set reg simm)    = setA reg simm
-- semanticsA (Store reg addr)  = store reg addr
-- semanticsA (Add reg addr)    = add reg addr
-- semanticsA (Jump simm)       = jump simm

-- semanticsS :: Instruction -> Semantics Selective MachineKey Value ()
-- semanticsS (JumpZero simm)   = jumpZero simm
-- semanticsS (LoadMI _ _)      = const (const Nothing)
-- semantics i                  = semanticsA i

-- -- | Monadic semantics may involve data dynamic analysis and must be executed
-- --   on a concrete machine state.
-- --
-- --   Note: Indirect memory access ('LoadMI') and conditional jump ('JumpZero')
-- --   instruction may be only assigned monadic semantics.
-- semanticsM :: Instruction -> Semantics Monad MachineKey Value ()
-- semanticsM (LoadMI reg addr) = loadMI reg addr
-- semanticsM i                 = semanticsS i

-- | Halt the execution.
--   Functor.
haltF :: Semantics Functor (SBV MachineKey) (SBV Value) ()
haltF read write = Just $
    write (literal $ F Halted) ((const 1) <$> read (literal $ F Halted))

-- | Halt the execution.
--   Applicative.
haltA :: Semantics Applicative (SBV MachineKey) (SBV Value) ()
haltA read write = Just $
    write (literal $ F Halted) (pure 1)

-- | Load a value from a memory location to a register.
--   Functor.
load :: Register -> MemoryAddress -> Semantics Functor (SBV MachineKey) (SBV Value) ()
load reg addr read write = Just $
    write (literal $ Reg reg) (read (literal $ Addr addr))

-- | Set a register value.
--   Functor.
setF :: Register -> SImm8 -> Semantics Functor (SBV MachineKey) (SBV Value) ()
setF reg simm read write = Just $
    write (literal $ Reg reg) ((const $ literal simm) <$> (read (literal $ Reg reg)))

-- | Set a register value.
--   Applicative.
setA :: Register -> SImm8 -> Semantics Applicative (SBV MachineKey) (SBV Value) ()
setA reg simm read write = Just $
    write (literal $ Reg reg) (pure . literal $ simm)

-- | Store a value from a register to a memory location.
--   Functor.
store :: Register -> MemoryAddress -> Semantics Functor (SBV MachineKey) (SBV Value) ()
store reg addr read write = Just $
    write (literal $ Addr addr) (read (literal $ Reg reg) )

-- | Add a value from memory location to one in a register.
--   Applicative.
add :: Register -> MemoryAddress -> Semantics Applicative (SBV MachineKey) (SBV Value) ()
add reg addr = \read write -> Just $
    let result = (+)    <$> read (literal $ Reg reg) <*> read (literal $ Addr addr)
        isZero = (.== literal 0) <$> result
    in  write (literal $ Reg reg) result *>
        write (literal $ F Zero) (boolToValue <$> isZero)

boolToValue = oneIf

-- | Unconditional jump.
--   Functor.
jump :: SImm8 -> Semantics Functor (SBV MachineKey) (SBV Value) ()
jump simm read write = Just $
    write (literal IC) (fmap (+ literal simm) (read . literal $ IC))

-- | Indirect memory access.
--   Monadic.
loadMI :: MachineKey -> MachineKey -> Semantics Monad (SBV MachineKey) (SBV Value) ()
loadMI reg addr read write = Just $ do
    addr' <- read (literal addr)
    write (literal reg) (read . valueToAddr $ addr')

valueToAddr :: SBV Value -> SBV MachineKey
valueToAddr = unsafeCoerce

-- | Jump if 'Zero' flag is set.
--   Selective.
jumpZero :: SImm8 -> Semantics Selective (SBV MachineKey) (SBV Value) ()
jumpZero simm read write = Just $
    ifS ((.==) <$> read (literal $ F Zero) <*> (pure . literal $ 1))
        (write (literal $ IC) ((+) <$> read (literal $ IC) <*> pure . literal $ simm))
        (pure ())

-- blockSemanticsA :: [Instruction] -> Semantics Applicative (SBV MachineKey) (SBV Value) ()
-- blockSemanticsA xs = \read write->
--     foldr (\x acc -> ((*>)) <$> acc <*> semanticsA x read write) nop xs
--     where nop = Just $ pure ()

-- blockSemanticsS :: [Instruction] -> Semantics Selective (SBV MachineKey) (SBV Value) ()
-- blockSemanticsS xs = \read write->
--     foldr (\x acc -> ((*>)) <$> acc <*> semanticsS x read write) nop xs
--     where nop = Just $ pure ()