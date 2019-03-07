{-# LANGUAGE LambdaCase #-}

module Machine.Semantics.SymbolicNew where


import qualified Data.Tree as Tree
import qualified Data.Map.Strict as Map
import Data.Monoid ((<>))
import Data.Maybe (fromJust)

import Control.Monad.State
import Machine.Types
import Machine.Types.Value
import Machine.Instruction
import Machine.Instruction.Decode
import Machine.Instruction.Encode
import Machine.Semantics
import Machine.Program hiding (readProgram)
import Machine.Semantics.Symbolic.Types

-- runModel :: Int -> SymState -> Trace
-- runModel steps state
--     | steps <= 0 = Tree.Node state []
--     | otherwise  = if halted then Tree.Node state [] else Tree.Node state children
--   where
--     halted    = (Map.!) (flags state) Halted /= (SConst 0)
--     newStates = symStep state
--     children  = runModel (steps - 1) <$> newStates

-- runModelMerge :: Int -> SymState -> Trace
-- runModelMerge steps state
--     | steps <= 0 = Tree.Node state []
--     | otherwise  = if halted then Tree.Node state [] else Tree.Node state [children]
--   where
--     halted    = (Map.!) (flags state) Halted /= (SConst 0)
--     newState = symStepMerge state
--     children  = runModelMerge (steps - 1) newState

-- | Instance of the Machine.Metalanguage read command for symbolic execution
readKey :: MachineKey -> State SymState Sym
readKey = \case
    Reg  reg  -> readRegister reg
    Addr addr -> readMemory   addr
    F    flag -> readFlag     flag
    IC        -> instructionCounter <$> get -- error "Machine.Semantics.Symbolic: Can't read IC"
    IR        -> SConst <$> readInstructionRegister
        -- error "Machine.Semantics.Symbolic: Can't read IR" -- readInstructionRegister
    Prog addr -> SConst <$> readProgram addr

-- -- | Instance of the Machine.Metalanguage write command for symbolic execution
-- writeKey :: MachineKey
--          -> State SymState Sym
--          -> State SymState ()
-- writeKey k v = case k of
--     Reg  reg  -> v >>= writeRegister reg
--     Addr addr -> v >>= writeMemory   addr
--     F    flag -> v >>= writeFlag flag
--     IC        -> v >>= \case
--         val -> modify $ \currentState -> currentState {instructionCounter = val}
--         -- _ -> error "Machine.Semantics.Symbolic.writeKey: symbolic IC is not supported"
--     IR        -> v >>= \case
--         (SConst val) -> writeInstructionRegister val
--         _ -> error "Machine.Semantics.Symbolic.writeKey: symbolic IR is not supported"
--     Prog _    -> error "Machine.Semantics.Symbolic: Can't write Program"

-- symStep :: SymState -> [SymState]
-- symStep state =
--     let (instrCode, fetched) = (flip runState) state $ do
--                                     fetchInstruction
--                                     incrementInstructionCounter
--                                     readInstructionRegister
--         i = decode instrCode
--     in (snd . ((flip runState) fetched)) <$> case i of
--           Halt ->           singleton . fromJust $ semanticsM i readKey writeKey
--           Load reg addr ->  singleton . fromJust $ semanticsM i readKey writeKey
--           LoadMI _ _ ->     error "LoadMI semantics not implemented."
--           Set reg value ->  singleton . fromJust $ semanticsM i readKey writeKey
--           Store reg addr -> singleton $
--               readRegister reg >>= writeMemory addr
--           Add reg addr -> singleton . fromJust $ semanticsM i readKey writeKey
--           Jump offset -> singleton . fromJust $ semanticsM i readKey writeKey
--           JumpZero offset ->
--             let isZero = SEq ((Map.!) (flags state) Zero) (SConst 0)
--             -- The computation branches and we return a list of two possible states:
--             in [ do appendConstraint isZero
--                     fromJust $ semanticsM (Jump offset) readKey writeKey
--                , appendConstraint (SNot isZero)
--                ]
--           Sub reg addr -> singleton . fromJust $ semanticsM i readKey writeKey
--           Mod reg addr -> singleton . fromJust $ semanticsM i readKey writeKey
--           Mul reg addr -> singleton . fromJust $ semanticsM i readKey writeKey
--           Div reg addr -> singleton . fromJust $ semanticsM i readKey writeKey
--           Abs reg      -> singleton . fromJust $ semanticsM i readKey writeKey
--     where singleton x = [x]

-- symStepMerge :: SymState -> SymState
-- symStepMerge state =
--     let (instrCode, fetched) = (flip runState) state $ do
--                                     fetchInstruction
--                                     incrementInstructionCounter
--                                     readInstructionRegister
--         i = decode instrCode
--     in (snd . ((flip runState) fetched)) $ case i of
--           Halt ->           fromJust $ semanticsM i readKey writeKey
--           Load reg addr ->  fromJust $ semanticsM i readKey writeKey
--           LoadMI _ _ ->     error "LoadMI semantics not implemented."
--           Set reg value ->  fromJust $ semanticsM i readKey writeKey
--           Store reg addr -> fromJust $ semanticsM i readKey writeKey
--           Add reg addr -> fromJust $ semanticsM i readKey writeKey
--           Jump offset -> fromJust $ semanticsM i readKey writeKey
--           JumpZero offset -> fromJust $ semanticsM i readKey writeKey -- undefined
--         --     let isZero = SEq ((Map.!) (flags state) Zero) (SConst 0)
--         --     -- The computation branches and we return a list of two possible states:
--         --     in [ do appendConstraint isZero
--         --             fromJust $ semanticsM (Jump offset) readKey writeKey
--         --        , appendConstraint (SNot isZero)
--         --        ]
--           Sub reg addr -> fromJust $ semanticsM i readKey writeKey
--           Mod reg addr -> fromJust $ semanticsM i readKey writeKey
--           Mul reg addr -> fromJust $ semanticsM i readKey writeKey
--           Div reg addr -> fromJust $ semanticsM i readKey writeKey
--           Abs reg      -> fromJust $ semanticsM i readKey writeKey

-- --------------------------------------------------------------------------------
-- ------------ Clock -------------------------------------------------------------
-- --------------------------------------------------------------------------------

-- -- | Advance the clock by a given number of clock cycles.
-- delay :: Clock -> State SymState ()
-- delay cycles =
--     modify $ \currentState ->
--         currentState {clock = clock currentState + cycles}

-- --------------------------------------------------------------------------------
-- ------------ Memory ------------------------------------------------------------
-- --------------------------------------------------------------------------------

-- -- | Write a new 'Value' to the given 'MemoryAddress'. We assume that it takes 2
-- -- clock cycles to access the memory in hardware.
-- writeMemory :: MemoryAddress -> Sym -> State SymState ()
-- writeMemory address value = do
--     delay 2
--     modify $ \currentState ->
--         currentState {memory =
--             Map.adjust (const value) (fromIntegral address) (memory currentState)}

-- -- | Lookup the 'Value' at the given 'MemoryAddress'. If the value has never
-- -- been initialised, this function returns 0. We assume that it
-- -- takes 2 clock cycles to access the memory in hardware.
-- readMemory :: MemoryAddress -> State SymState Sym
-- readMemory address = do
--     currentState <- get
--     delay 2
--     pure $ (Map.!) (memory currentState) (fromIntegral address)

-- -- --------------------------------------------------------------------------------
-- -- ------------ Registers ---------------------------------------------------------
-- -- --------------------------------------------------------------------------------

-- -- | Lookup the 'Val ue' in a given 'Register'. If the register has never been
-- -- initialised, this function returns 0. We assume that it
-- -- takes 1 clock cycles to access a register in hardware.
-- readRegister :: Register -> State SymState Sym
-- readRegister register = do
--     s <- get
--     delay 1
--     pure $ (Map.!) (registers s) register

-- -- | Write a new 'Value' to a given 'Register'.
-- --   We assume that it takes 1 clock cycle to access a register in hardware.
-- writeRegister :: Register -> Sym -> State SymState ()
-- writeRegister register value = do
--     delay 1
--     modify $ \currentState ->
--         currentState {registers = Map.adjust (const value) register (registers currentState)}

-- -- --------------------------------------------------------------------------------
-- -- ------------ Flags ---------------------------------------------------------
-- -- --------------------------------------------------------------------------------

-- -- | Lookup the value of a given 'Flag'. If the flag is not currently assigned
-- -- any value, it is assumed to be 'False'.
-- readFlag :: Flag -> State SymState Sym
-- readFlag flag = do
--     currentState <- get
--     pure $ (Map.!) (flags currentState) flag

-- -- | Set a given 'Flag' to the specified Boolean value.
-- --   We assume that it takes 1 clock cycle to access
-- --   the flag register in hardware.
-- writeFlag :: Flag -> Sym -> State SymState ()
-- writeFlag flag value = do
--     delay 1
--     modify $ \currentState ->
--         currentState {
--             flags = Map.adjust (const value) flag (flags currentState)}

-- -- --------------------------------------------------------------------------------
-- -- ------------ Program -----------------------------------------------------------
-- -- --------------------------------------------------------------------------------
-- -- | Increment the instruction counter.
-- incrementInstructionCounter :: State SymState ()
-- incrementInstructionCounter =
--     modify $ \currentState ->
--         currentState {instructionCounter = instructionCounter currentState + 1}

-- fetchInstruction :: State SymState ()
-- fetchInstruction = do
--     ic <- instructionCounter <$> get
--     case ic of
--         SConst v -> readProgram v >>= writeInstructionRegister
--         _        -> error "Machine.Semantics.Symbolic.writeKey: symbolic IC is not supported"

-- readProgram :: InstructionAddress -> State SymState (InstructionCode)
-- readProgram addr = do
--     currentState <- get
--     delay 1
--     pure . snd $ (!!) (program currentState) (fromIntegral addr)

-- readInstructionRegister :: State SymState InstructionCode
-- readInstructionRegister = instructionRegister <$> get

-- writeInstructionRegister :: InstructionCode -> State SymState ()
-- writeInstructionRegister instruction =
--     modify $ \currentState ->
--         currentState {instructionRegister = instruction}

--------------------------------------------------------------------------------
-------------- Path Constraints ------------------------------------------------
--------------------------------------------------------------------------------

appendConstraint :: Sym -> State SymState ()
appendConstraint c = modify $ \state ->
    state { pathConstraintList = c : pathConstraintList state }