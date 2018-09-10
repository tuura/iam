{-# LANGUAGE LambdaCase #-}

module Machine.Semantics.Symbolic where


import qualified Data.Tree as Tree
import qualified Data.Map.Strict as Map
import Data.Monoid ((<>))

import Control.Monad.State
import Machine.Types
import Machine.Types.Value
import Machine.Instruction
import Machine.Instruction.Decode
import Machine.Instruction.Encode
import Machine.Program hiding (readProgram)
import Machine.Semantics.Symbolic.Types

runModel :: Int -> SymState -> Trace
runModel steps state
    | steps <= 0 = Tree.Node state []
    | otherwise  = if halted then Tree.Node state [] else Tree.Node state children
  where
    halted    = (Map.!) (flags state) Halted /= (SConst 0)
    newStates = symStep state
    children  = runModel (steps - 1) <$> newStates

symStep :: SymState -> [SymState]
symStep state =
    let (instrCode, fetched) = (flip runState) state $ do
                                    fetchInstruction
                                    incrementInstructionCounter
                                    readInstructionRegister
    in (snd . ((flip runState) fetched)) <$> case decode instrCode of
          Halt -> singleton $ writeFlag Halted (SConst 1)
          Load reg addr -> singleton $ do
              x <- readMemory addr
              writeRegister reg x
          LoadMI _ _ -> error "LoadMI semantics not implemented."
          Set reg value -> singleton $ do
              writeRegister reg (SConst . fromIntegral $ value)
          Store reg addr -> singleton $
              readRegister reg >>= writeMemory addr
          Add reg addr -> singleton $ do
              x <- readRegister reg
              y <- readMemory addr
              let result = SAdd x y
              writeRegister reg result
              writeFlag Zero result
         --   (((si_b > 0) && (si_a > (INT_MAX - si_b))) ||
         --    ((si_b < 0) && (si_a < (INT_MIN - si_b)))) {
              let o1 = SGt y (SConst 0)
                  o2 = SGt x (SSub (SConst maxBound) y)
                  o3 = SLt y (SConst 0)
                  o4 = SLt x (SSub (SConst minBound) y)
                  o  = SOr (SAnd o1 o2)
                           (SAnd o3 o4)
              writeFlag Overflow o
          Jump offset -> singleton $
              modify $ \state ->
                state { instructionCounter =
                            instructionCounter state + (fromIntegral offset)
                      }
          JumpZero offset -> let isZero = SEq ((Map.!) (flags state) Zero) (SConst 0) in
          -- The computation branches and we return a list of two possible states:
                              [ modify $ \state -> -- flag 'Zero@ is set and we jump
                                  state { instructionCounter =
                                             instructionCounter state + (fromIntegral offset)
                                        , pathConstraintList = isZero : pathConstraintList state
                                        }
                              , modify $ \state -> -- otherwise don't jump
                                  state { pathConstraintList = SNot isZero : pathConstraintList state
                                        }
                              ]
          Sub reg addr -> singleton $ do
              x <- readRegister reg
              y <- readMemory addr
              let result = SSub x y
              writeRegister reg result
              writeFlag Zero result
          Mod reg addr -> singleton $ do
              x <- readRegister reg
              y <- readMemory addr
              let result = SMod x y
              writeRegister reg result
              writeFlag Zero result
        --   _ -> error "semantics undefined"
    where singleton x = [x]
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
readFlag :: Flag -> State SymState Sym
readFlag flag = do
    currentState <- get
    pure $ (Map.!) (flags currentState) flag

-- | Set a given 'Flag' to the specified Boolean value.
--   We assume that it takes 1 clock cycle to access
--   the flag register in hardware.
writeFlag :: Flag -> Sym -> State SymState ()
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