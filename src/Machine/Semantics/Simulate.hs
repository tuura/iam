{-# LANGUAGE LambdaCase #-}

module Machine.Semantics.Simulate (
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
import qualified Data.Map.Strict as Map
import Machine.Types
import Machine.Instruction
import Machine.Program hiding (readProgram)
import Machine.Semantics

-- | The state of a Iam machine
data MachineState = MachineState
    { registers           :: RegisterBank
    , instructionCounter  :: InstructionAddress
    , instructionRegister :: InstructionCode
    , flags               :: Flags
    , memory              :: Memory
    , program             :: Program
    , clock               :: Clock
    } deriving (Show)

emptyRegisters :: RegisterBank
emptyRegisters = Map.fromList $ zip [R0, R1, R2, R3] [0, 0..]

emptyFlags :: Flags
emptyFlags = Map.fromList $ zip [Zero, Overflow, Halted] [0, 0..]

initialiseMemory :: [(MemoryAddress, Value)] -> Memory
initialiseMemory m =
    let blankMemory = Map.fromList $ zip [0..1023] [0, 0..]
    in foldr (\(addr, value) acc -> Map.adjust (const value) addr acc) blankMemory m

dumpMemory :: Value -> Value -> Memory -> [Value]
dumpMemory from to m = map ((Map.!) m) [from..to]

boot :: Program -> Memory -> MachineState
boot prog mem = MachineState { registers = emptyRegisters
                             , instructionCounter = 0
                             , instructionRegister = 0 -- encode $ Jump 0
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
    halted    = (Map.!) (flags state) Halted /= 0
    nextState = case executeInstruction readKey writeKey of
                    [t, f] ->
                        let isZero = (==) ((Map.!) (flags state) Zero) 0
                        in if isZero then snd $ runState t state
                                     else snd $ runState f state
                    [action] -> snd $ runState action state
                    []       -> error "incorrect instruction semantics"

-- | Instance of the Machine.Metalanguage read command for symbolic execution
readKey :: MachineKey
        -> State MachineState Value
readKey = \case
    Reg  reg  -> readRegister reg
    Addr addr -> readMemory   addr
    F    flag -> readFlag     flag
    IC        -> instructionCounter <$> get
    IR        -> readInstructionRegister
    Prog addr -> readProgram addr

-- | Instance of the Machine.Metalanguage write command for symbolic execution
writeKey :: MachineKey
         -> State MachineState Value
         -> State MachineState ()
writeKey k v = case k of
    Reg  reg  -> v >>= writeRegister reg
    Addr addr -> v >>= writeMemory   addr
    F    flag -> v >>= writeFlag flag
    IC        -> do
        ic' <- v
        modify $ \currentState -> currentState {instructionCounter = ic'}
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
writeMemory :: MemoryAddress -> Value -> State MachineState ()
writeMemory address value = do
    delay 2
    modify $ \currentState ->
        currentState {memory =
            Map.adjust (const value) address (memory currentState)}

-- | Lookup the 'Value' at the given 'MemoryAddress'. If the value has never
-- been initialised, this function returns 0. We assume that it
-- takes 2 clock cycles to access the memory in hardware.
readMemory :: MemoryAddress -> State MachineState (Value)
readMemory address = do
    currentState <- get
    delay 2
    pure $ (Map.!) (memory currentState) address

toMemoryAddress :: Value -> State MachineState (MemoryAddress)
toMemoryAddress value = do
    let valid = value <= 255
    -- return $ fromBitsLE (take 8 $ blastLE value)
    pure value

--------------------------------------------------------------------------------
------------ Registers ---------------------------------------------------------
--------------------------------------------------------------------------------

-- | Lookup the 'Value' in a given 'Register'. If the register has never been
-- initialised, this function returns 0. We assume that it
-- takes 1 clock cycles to access a register in hardware.
readRegister :: Register -> State MachineState (Value)
readRegister register = do
    s <- get
    delay 1
    pure $ (Map.!) (registers s) register

-- | Write a new 'Value' to a given 'Register'.
--   We assume that it takes 1 clock cycle to access a register in hardware.
writeRegister :: Register -> Value -> State MachineState ()
writeRegister register value = do
    delay 1
    modify $ \currentState ->
        currentState {registers = Map.adjust (const value) register (registers currentState)}

--------------------------------------------------------------------------------
------------ Flags ---------------------------------------------------------
--------------------------------------------------------------------------------

-- | Lookup the value of a given 'Flag'. If the flag is not currently assigned
-- any value, it is assumed to be 'False'.
readFlag :: Flag -> State MachineState (Value)
readFlag flag = do
    currentState <- get
    pure $ (Map.!) (flags currentState) flag

-- | Set a given 'Flag' to the specified Boolean value.
--   We assume that it takes 1 clock cycle to access
--   the flag register in hardware.
writeFlag :: Flag -> (Value) -> State MachineState ()
writeFlag flag value = do
    delay 1
    modify $ \currentState ->
        currentState {
            flags = Map.adjust (const value) flag (flags currentState)}

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
    get >>= readProgram . instructionCounter >>= writeInstructionRegister

readProgram :: InstructionAddress -> State MachineState (InstructionCode)
readProgram addr = do
    currentState <- get
    delay 1
    pure . snd $ (!!) (program currentState) (fromIntegral addr)

readInstructionRegister :: State MachineState (InstructionCode)
readInstructionRegister = instructionRegister <$> get

writeInstructionRegister :: (InstructionCode) -> State MachineState ()
writeInstructionRegister instruction =
    modify $ \currentState ->
        currentState {instructionRegister = instruction}