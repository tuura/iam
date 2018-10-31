{-# LANGUAGE DeriveFunctor #-}

module Machine.Semantics.Symbolic.SymEngine where

import qualified Data.Map.Strict as Map
import Control.Selective
import Control.Monad (ap)
import Control.Monad.State
import Control.Monad.Writer.Strict
import Machine.Types
import Machine.Instruction
import Machine.Semantics.Symbolic.Types

-- | The Symbolic Execution Engine maintains the state of the machine and a list
--   of path constraints.
data SymEngine a = SymEngine
    { runSymEngine :: SymState -> [((a, SymState), [Sym])] }
    deriving Functor

-- | A standard 'Applicative' instance available for any 'Monad'.
instance Applicative SymEngine where
    pure  = return
    (<*>) = ap

instance Monad SymEngine where
    return a       = SymEngine $ \s -> [((a, s), [])]
    SymEngine r >>= f = SymEngine $ \s ->
        let outcomes = r s
        in concat $ map (\((result, state), _) -> runSymEngine (f result) state) outcomes

getState :: SymEngine SymState
getState = SymEngine $ \s -> [((s, s), [])]

putState :: SymState -> SymEngine ()
putState s = SymEngine $ \_ -> [(((), s), [])]

-- | Transform the current 'State' by applying a given transformation function.
modifyState :: (SymState -> SymState) -> SymEngine ()
modifyState f = SymEngine $ \s -> [(((), f s), [])]

appendConstraints :: [Sym] -> SymEngine ()
appendConstraints cs = SymEngine $ \s -> [(((), s), cs)]

ite :: SymEngine Sym -> SymEngine a -> SymEngine a -> SymEngine a
ite i t e = SymEngine $ \s ->
    let [((cond, s'), constraints)] = runSymEngine i s
    in runSymEngine (appendConstraints [cond] *> t) s' ++
       runSymEngine (appendConstraints [SNot cond] *> e) s'

--------------------------------------------------------------------------------
----------- The IAM microcommands in terms of SymEngine
--------------------------------------------------------------------------------

---------- Clock -------------------------------------------------------------
--------------------------------------------------------------------------------

-- | Advance the clock by a given number of clock cycles.
delay :: Clock -> SymEngine ()
delay cycles =
    modifyState $ \currentState ->
        currentState {clock = clock currentState + cycles}

--------------------------------------------------------------------------------
------------ Memory ------------------------------------------------------------
--------------------------------------------------------------------------------

-- | Write a new 'Value' to the given 'MemoryAddress'. We assume that it takes 2
-- clock cycles to access the memory in hardware.
writeMemory :: MemoryAddress -> Sym -> SymEngine ()
writeMemory address value = do
    delay 2
    modifyState $ \currentState ->
        currentState {memory =
            Map.adjust (const value) (fromIntegral address) (memory currentState)}

-- | Lookup the 'Value' at the given 'MemoryAddress'. If the value has never
-- been initialised, this function returns 0. We assume that it
-- takes 2 clock cycles to access the memory in hardware.
readMemory :: MemoryAddress -> SymEngine Sym
readMemory address = do
    currentState <- getState
    delay 2
    pure $ (Map.!) (memory currentState) (fromIntegral address)

-- --------------------------------------------------------------------------------
-- ------------ Registers ---------------------------------------------------------
-- --------------------------------------------------------------------------------

-- | Lookup the 'Val ue' in a given 'Register'. If the register has never been
-- initialised, this function returns 0. We assume that it
-- takes 1 clock cycles to access a register in hardware.
readRegister :: Register -> SymEngine Sym
readRegister register = do
    s <- getState
    delay 1
    pure $ (Map.!) (registers s) register

-- | Write a new 'Value' to a given 'Register'.
--   We assume that it takes 1 clock cycle to access a register in hardware.
writeRegister :: Register -> Sym -> SymEngine ()
writeRegister register value = do
    delay 1
    modifyState $ \currentState ->
        currentState {registers = Map.adjust (const value) register (registers currentState)}

-- --------------------------------------------------------------------------------
-- ------------ Flags ---------------------------------------------------------
-- --------------------------------------------------------------------------------

-- | Lookup the value of a given 'Flag'. If the flag is not currently assigned
-- any value, it is assumed to be 'False'.
readFlag :: Flag -> SymEngine Sym
readFlag flag = do
    currentState <- getState
    pure $ (Map.!) (flags currentState) flag

-- | Set a given 'Flag' to the specified Boolean value.
--   We assume that it takes 1 clock cycle to access
--   the flag register in hardware.
writeFlag :: Flag -> Sym -> SymEngine ()
writeFlag flag value = do
    delay 1
    modifyState $ \currentState ->
        currentState {
            flags = Map.adjust (const value) flag (flags currentState)}

-- --------------------------------------------------------------------------------
-- ------------ Program -----------------------------------------------------------
-- --------------------------------------------------------------------------------
-- | Increment the instruction counter.
incrementInstructionCounter :: SymEngine ()
incrementInstructionCounter =
    modifyState $ \currentState ->
        currentState {instructionCounter = instructionCounter currentState + 1}

fetchInstruction :: SymEngine ()
fetchInstruction = do
    ic <- instructionCounter <$> getState
    case ic of
        v -> readProgram v >>= writeInstructionRegister
        _        -> error "Machine.Semantics.Symbolic.writeKey: symbolic IC is not supported"

readProgram :: InstructionAddress -> SymEngine (InstructionCode)
readProgram addr = do
    currentState <- getState
    delay 1
    pure . snd $ (!!) (program currentState) (fromIntegral addr)

readInstructionRegister :: SymEngine InstructionCode
readInstructionRegister = instructionRegister <$> getState

writeInstructionRegister :: InstructionCode -> SymEngine ()
writeInstructionRegister instruction =
    modifyState $ \currentState ->
        currentState {instructionRegister = instruction}

--------------------------------------------------------------------------------
-------------- Instruction Semantics in terms of SymEngine ---------------------
--------------------------------------------------------------------------------

-- | Halt the execution.
--   Applicative.
halt :: SymEngine ()
halt = writeFlag Halted (SConst 1)

-- | Load a value from a memory location to a register.
--   Functor.
load :: Register -> MemoryAddress -> SymEngine ()
load reg addr =
    readMemory addr >>= writeRegister reg

-- | Set a register v
-- | Set a register value.
--   Applicative.
setA :: Register -> Value -> SymEngine ()
setA reg simm =
    writeRegister reg (SConst simm)

-- | Store a value from a register to a memory location.
--   Functor.
store :: Register -> MemoryAddress -> SymEngine ()
store reg addr =
    readRegister reg >>= writeMemory addr

-- | Add a value from memory location to one in a register.
--   Applicative.
add :: Register -> MemoryAddress -> SymEngine ()
add reg addr = do
    result <- (+) <$> readRegister reg <*> readMemory addr
    writeFlag Zero result
    writeRegister reg result

-- -- | Add a value from memory location to one in a register. Tracks overflow.
-- --   Selective.
-- addS :: Register -> -> Semantics Selective MachineKey a ()
-- addS reg addr =  ->
--     let arg1   = read (Reg reg)
--         arg2   = read (Addr addr)
--         result = (+) <$> read (Reg reg) <*> read (Addr addr)
--         o1 = gt <$> arg2 <*> pure 0
--         o2 = gt <$> arg1 <*> ((-) <$> pure maxBound <*> arg2)
--         o3 = lt <$> arg2 <*> pure 0
--         o4 = lt <$> arg1 <*> ((-) <$> pure minBound <*> arg2)
--         o  = or <$> (and <$> o1 <*> o2)
--                 <*> (and <$> o3 <*> o4)
--     in  write (F Overflow) o *>
--         write (Reg reg) result

-- | Sub a value from memory location to one in a register.
--   Applicative.
sub :: Register -> MemoryAddress -> SymEngine ()
sub reg addr = do
    result <- (-) <$> readRegister reg <*> readMemory addr
    writeFlag Zero  result
    writeRegister reg result

-- -- | Multiply a value from memory location to one in a register.
-- --   Applicative.
-- mul :: Register -> MemoryAddress -> SymEngine ()
-- mul reg addr =  ->
--     let result = (*) <$> read (Reg reg) <*> read (Addr addr)
--     in  write (F Zero)  result *>
--         write (Reg reg) result

-- -- | Subtract a value from memory location to one in a register.
-- --   Applicative.
-- div :: Register -> MemoryAddress -> SymEngine ()
-- div reg addr =  ->
--     let result = Value.div <$> read (Reg reg) <*> read (Addr addr)
--     in  write (F Zero)  result *>
--         write (Reg reg) result

mod :: Register -> MemoryAddress -> SymEngine ()
mod reg addr = do
    result <- SMod <$> readRegister reg <*> readMemory addr
    writeFlag Zero  result
    writeRegister reg result

-- abs :: -> SymEngine ()
-- abs reg =  ->
--     let result = Prelude.abs <$> read (Reg reg)
--     in  write (Reg reg) result

-- | Unconditional jump.
--   Functor.
jump :: SImm8 -> SymEngine ()
jump simm =
    sequence_ (replicate (fromIntegral simm) incrementInstructionCounter)

-- -- | Indirect memory access.
-- --   Monadic.
-- loadMI :: Register -> MemoryAddress -> Semantics Monad MachineKey a ()
-- loadMI reg addr = undefined
--     -- do
--     -- addr' <- read (Addr addr)
--     -- write (Reg reg) (read (Addr addr'))

jumpZero :: SImm8 -> SymEngine ()
jumpZero simm =
    let isZero = SEq <$> (readFlag Zero) <*> pure (SConst 0)
    in ite isZero (pure ()) (pure ())

--------------------------------------------------------------------------------
------------------ Examples ----------------------------------------------------
--------------------------------------------------------------------------------

gcdExample :: IO ()
gcdExample = do
    let prog = unsafePerformIO . readProgram $ "examples/gcd.asm"
        steps = 10
        -- x = SConst 2 -- SAny 0
        -- y = SConst 3 -- SAny 1
        x = SAny 0
        y = SAny 1
        mem = initialiseMemory [(0, x), (1, y)]
        initialState = boot prog mem
        trace = runModelMerge steps initialState
    putStrLn $ Tree.drawTree $ fmap show $ trace