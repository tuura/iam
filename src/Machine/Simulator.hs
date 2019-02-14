{-# LANGUAGE LambdaCase, GADTs, TypeFamilies, RankNTypes #-}

module Machine.Simulator (
    -- * State of IAM machine
    MachineState (..),

    -- * Create initial state
    initialiseMemory, boot,

    -- * Dump a chunk of memory
    dumpMemory,

    -- * Simulate the machine execution
    runModel
    ) where

import Control.Monad.State
import Prelude hiding (Monad)
import qualified Data.Map.Strict as Map
import FS
import Machine.Types
import Machine.Instruction
import Machine.Program hiding (readProgram)
import Machine

-- | The state of a Iam machine
data MachineState = MachineState
    { registers           :: RegisterBank
    , instructionCounter  :: InstructionAddress
    , instructionRegister :: Instruction
    , flags               :: Flags
    , memory              :: Memory
    , program             :: Program
    , clock               :: Clock
    } deriving (Show)

emptyRegisters :: RegisterBank
emptyRegisters = Map.fromList $ zip [R0, R1, R2, R3] [0, 0..]

emptyFlags :: Flags
emptyFlags = Map.fromList $ zip [Zero, Overflow, Halted] (repeat False)

initialiseMemory :: [(MemoryAddress, MachineValue)] -> Memory
initialiseMemory m =
    let blankMemory = Map.fromList $ zip [0..1023] [0, 0..]
    in foldr (\(addr, value) acc -> Map.adjust (const value) addr acc) blankMemory m

dumpMemory :: MachineValue -> MachineValue -> Memory -> [MachineValue]
dumpMemory from to m = map ((Map.!) m) [from..to]

boot :: Program -> Memory -> MachineState
boot prog mem = MachineState { registers = emptyRegisters
                             , instructionCounter = 0
                             , instructionRegister = Instruction $ Jump 0
                             , program = prog
                             , flags = emptyFlags
                             , memory = mem
                             , clock = 0
                             }

--------------------------------------------------------------------------------

runModel :: Int -> MachineState -> MachineState
runModel steps state
    | steps <= 0 = state
    | otherwise  = if halted then state else runModel (steps - 1) nextState
  where
    halted    = (Map.!) (flags state) Halted
    nextState = snd $ runState (executeInstruction readKey writeKey) state

-- | Instance of the Machine.Metalanguage read command for simulation
readKey :: MachineKey a
        -> State MachineState a
readKey key = case key of
    Reg  reg  -> readRegister reg
    Addr addr -> readMemory   addr
    F    flag -> readFlag     flag
    IC        -> instructionCounter <$> get
    IR        -> readInstructionRegister
    Prog addr -> readProgram addr

-- | Instance of the Machine.Metalanguage write command for simulation
writeKey :: MachineKey a
         -> State MachineState a
         -> State MachineState a
writeKey k v = case k of
    Reg  reg  -> v >>= writeRegister reg
    Addr addr -> v >>= writeMemory   addr
    F    flag -> v >>= writeFlag flag
    IC        -> do
        ic' <- v
        modify $ \currentState -> currentState {instructionCounter = ic'}
        pure ic'
    IR        -> v >>= writeInstructionRegister
    Prog addr -> error "Machine.Semantics.Symbolic: Can't write Program"

--------------------------------------------------------------------------------
------------ Clock -------------------------------------------------------------
--------------------------------------------------------------------------------

-- | Advance the clock by a given number of clock cycles.
delay :: Clock -> State MachineState ()
delay cycles =
    modify $ \currentState ->
        currentState {clock = clock currentState + cycles}

--------------------------------------------------------------------------------
------------ Memory ------------------------------------------------------------
--------------------------------------------------------------------------------

-- | Write a new 'Value' to the given 'MemoryAddress'. We assume that it takes 2
-- clock cycles to access the memory in hardware.
writeMemory :: MemoryAddress -> MachineValue -> State MachineState MachineValue
writeMemory address value = do
    delay 2
    modify $ \currentState ->
        currentState {memory =
            Map.adjust (const value) address (memory currentState)}
    pure value

-- | Lookup the 'Value' at the given 'MemoryAddress'. If the value has never
-- been initialised, this function returns 0. We assume that it
-- takes 2 clock cycles to access the memory in hardware.
readMemory :: MemoryAddress -> State MachineState MachineValue
readMemory address = do
    currentState <- get
    delay 2
    pure $ (Map.!) (memory currentState) address

toMemoryAddress :: MachineValue -> State MachineState MemoryAddress
toMemoryAddress value = do
    -- let valid = value <= 255
    -- return $ fromBitsLE (take 8 $ blastLE value)
    pure value

--------------------------------------------------------------------------------
------------ Registers ---------------------------------------------------------
--------------------------------------------------------------------------------

-- | Lookup the 'Value' in a given 'Register'. If the register has never been
-- initialised, this function returns 0. We assume that it
-- takes 1 clock cycles to access a register in hardware.
readRegister :: Register -> State MachineState MachineValue
readRegister register = do
    s <- get
    delay 1
    pure $ (Map.!) (registers s) register

-- | Write a new 'Value' to a given 'Register'.
--   We assume that it takes 1 clock cycle to access a register in hardware.
writeRegister :: Register -> MachineValue -> State MachineState MachineValue
writeRegister register value = do
    delay 1
    modify $ \currentState ->
        currentState {registers = Map.adjust (const value) register (registers currentState)}
    pure value

--------------------------------------------------------------------------------
------------ Flags ---------------------------------------------------------
--------------------------------------------------------------------------------

-- | Lookup the value of a given 'Flag'. If the flag is not currently assigned
-- any value, it is assumed to be 'False'.
readFlag :: Flag -> State MachineState Bool
readFlag flag = do
    currentState <- get
    pure $ (Map.!) (flags currentState) flag

-- | Set a given 'Flag' to the specified Boolean value.
--   We assume that it takes 1 clock cycle to access
--   the flag register in hardware.
writeFlag :: Flag -> Bool -> State MachineState Bool
writeFlag flag value = do
    delay 1
    modify $ \currentState ->
        currentState {
            flags = Map.adjust (const value) flag (flags currentState)}
    pure value

--------------------------------------------------------------------------------
------------ Program -----------------------------------------------------------
--------------------------------------------------------------------------------
-- | Increment the instruction counter.
incrementInstructionCounter :: State MachineState ()
incrementInstructionCounter =
    modify $ \currentState ->
        currentState {instructionCounter = instructionCounter currentState + 1}

fetchInstruction :: State MachineState ()
fetchInstruction =
    get >>= readProgram . instructionCounter >>= writeInstructionRegister >> pure ()

readProgram :: InstructionAddress -> State MachineState Instruction
readProgram addr = do
    currentState <- get
    delay 1
    pure . snd $ (!!) (program currentState) (fromIntegral addr)

readInstructionRegister :: State MachineState Instruction
readInstructionRegister = instructionRegister <$> get

writeInstructionRegister :: Instruction
                         -> State MachineState Instruction
writeInstructionRegister instruction = do
    modify $ \currentState ->
        currentState {instructionRegister = instruction}
    pure instruction