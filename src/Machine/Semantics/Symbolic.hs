{-# LANGUAGE LambdaCase #-}

module Machine.Semantics.Symbolic where


import qualified Data.Tree as Tree
import qualified Data.Map.Strict as Map
import Data.Monoid ((<>))
import Text.Pretty.Simple
import Control.Monad.State
import Machine.Types
import Machine.Instruction
import Machine.Instruction.Decode
import Machine.Instruction.Encode
import Machine.Program hiding (readProgram)
import Data.Word (Word8)

-- | Symbolic expressions
data Sym = SAdd Sym Sym
         | SConst Value
         | SAnd Sym Sym
         | SAny Int     -- Any value or the set of all values
         deriving (Eq, Ord)

instance Show Sym where
    show (SAdd x y) = "(" <> show x <> " + " <> show y <> ")"
    show (SConst x) = show x
    show (SAnd x y) = "(" <> show x <> " & " <> show y <> ")"
    show (SAny n  ) = "val_" ++ show n

data SymState = SymState { registers         :: Map.Map Register Sym
                         , instructionCounter :: InstructionAddress
                         , instructionRegister :: InstructionCode
                         , flags :: Flags
                         , memory :: Map.Map Word8 Sym
                         , program :: Program
                         , clock :: Clock

                         , uniqueVarCounter  :: Int
                         , pathConstraintList :: [Sym]
                         }

instance Show SymState where
    show state = "Registers: " <> show (registers state)

type Trace = Tree.Tree SymState

runModel :: Int -> SymState -> Trace
runModel steps state
    | steps <= 0 = Tree.Node state []
    | otherwise  = if halted then Tree.Node state [] else Tree.Node state children
  where
    halted    = (Map.!) (flags state) Halted /= 0
    newStates = symStep state
    children  = runModel (steps - 1) <$> newStates

symStep :: SymState -> [SymState]
symStep state =
    let (instrCode, fetched) = (flip runState) state $ do
                                    fetchInstruction
                                    incrementInstructionCounter
                                    readInstructionRegister
    in pure . snd $ (flip runState) fetched $ case decode instrCode of
          Halt -> writeFlag Halted 1
          Load reg addr -> do
              x <- readMemory addr
              writeRegister reg x
          Add reg addr -> do
              x <- readRegister reg
              y <- readMemory addr
              writeRegister reg (SAdd x y)
          _ -> error "semantics undefined"

emptyRegisters :: Map.Map Register Sym
emptyRegisters = Map.fromList $ zip [R0, R1, R2, R3] (map SConst [0, 0..])

emptyFlags :: Flags
emptyFlags = Map.fromList $ zip [Zero, Overflow, Halted] [0, 0..]

initialiseMemory :: [(MemoryAddress, Sym)] -> Map.Map Word8 Sym
initialiseMemory vars =
    let blankMemory = Map.fromList $ zip [0..255] (map SConst [0, 0..])
    in foldr (\(addr, value) acc -> Map.adjust (const value) (fromIntegral addr) acc) blankMemory vars

boot :: Program -> Map.Map Word8 Sym -> SymState
boot prog mem = SymState { registers = emptyRegisters
                         , instructionCounter = 0
                         , instructionRegister = 0 -- encode $ Jump 0
                         , program = prog
                         , flags = emptyFlags
                         , memory = mem
                         , clock = 0

                         , uniqueVarCounter  = 0
                         , pathConstraintList = []
                         }

--------------------------------------------------------------------------------

addExample :: IO ()
addExample = do
    print "Add works as '+'."
    let prog = [ (0, encode $ Load R0 0)
               , (1, encode $ Add  R0 1)
               , (2, encode Halt)]
        steps = 10
        machineAdd x y = let mem = initialiseMemory [(0, x), (1, y)]
                             initialState = boot prog mem
                             trace = runModel steps initialState
                         in  trace
    putStrLn $ Tree.drawTree $ fmap show $ machineAdd (SAny 0) (SAny 1)
--------------------------------------------------------------------------------
------------ Clock -------------------------------------------------------------
--------------------------------------------------------------------------------

-- | Advance the clock by a given number of clock cycles.
delay :: Clock -> State SymState ()
delay cycles =
    modify $ \currentState ->
        currentState {clock = clock currentState + cycles}

--------------------------------------------------------------------------------
------------ Memory ------------------------------------------------------------
--------------------------------------------------------------------------------

-- | Write a new 'Value' to the given 'MemoryAddress'. We assume that it takes 2
-- clock cycles to access the memory in hardware.
writeMemory :: MemoryAddress -> Sym -> State SymState ()
writeMemory address value = do
    delay 2
    modify $ \currentState ->
        currentState {memory =
            Map.adjust (const value) (fromIntegral address) (memory currentState)}

-- | Lookup the 'Value' at the given 'MemoryAddress'. If the value has never
-- been initialised, this function returns 0. We assume that it
-- takes 2 clock cycles to access the memory in hardware.
readMemory :: MemoryAddress -> State SymState Sym
readMemory address = do
    currentState <- get
    delay 2
    pure $ (Map.!) (memory currentState) (fromIntegral address)

-- --------------------------------------------------------------------------------
-- ------------ Registers ---------------------------------------------------------
-- --------------------------------------------------------------------------------

-- | Lookup the 'Value' in a given 'Register'. If the register has never been
-- initialised, this function returns 0. We assume that it
-- takes 1 clock cycles to access a register in hardware.
readRegister :: Register -> State SymState Sym
readRegister register = do
    s <- get
    delay 1
    pure $ (Map.!) (registers s) register

-- | Write a new 'Value' to a given 'Register'.
--   We assume that it takes 1 clock cycle to access a register in hardware.
writeRegister :: Register -> Sym -> State SymState ()
writeRegister register value = do
    delay 1
    modify $ \currentState ->
        currentState {registers = Map.adjust (const value) register (registers currentState)}

-- --------------------------------------------------------------------------------
-- ------------ Flags ---------------------------------------------------------
-- --------------------------------------------------------------------------------

-- | Lookup the value of a given 'Flag'. If the flag is not currently assigned
-- any value, it is assumed to be 'False'.
readFlag :: Flag -> State SymState Value
readFlag flag = do
    currentState <- get
    pure $ (Map.!) (flags currentState) flag

-- | Set a given 'Flag' to the specified Boolean value.
--   We assume that it takes 1 clock cycle to access
--   the flag register in hardware.
writeFlag :: Flag -> Value -> State SymState ()
writeFlag flag value = do
    delay 1
    modify $ \currentState ->
        currentState {
            flags = Map.adjust (const value) flag (flags currentState)}

-- --------------------------------------------------------------------------------
-- ------------ Program -----------------------------------------------------------
-- --------------------------------------------------------------------------------
-- | Increment the instruction counter.
incrementInstructionCounter :: State SymState ()
incrementInstructionCounter =
    modify $ \currentState ->
        currentState {instructionCounter = instructionCounter currentState + 1}

fetchInstruction :: State SymState ()
fetchInstruction =
    get >>= readProgram . instructionCounter >>= writeInstructionRegister

readProgram :: InstructionAddress -> State SymState (InstructionCode)
readProgram addr = do
    currentState <- get
    delay 1
    pure . snd $ (!!) (program currentState) (fromIntegral addr)

readInstructionRegister :: State SymState (InstructionCode)
readInstructionRegister = instructionRegister <$> get

writeInstructionRegister :: (InstructionCode) -> State SymState ()
writeInstructionRegister instruction =
    modify $ \currentState ->
        currentState {instructionRegister = instruction}