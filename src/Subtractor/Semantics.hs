{-# LANGUAGE BinaryLiterals #-}

module Subtractor.Semantics where

import Data.SBV
import Control.Monad (when)
import Control.Monad.State.Strict (modify, runState, get, put)
import Subtractor.Types
import Subtractor.Verification
import Subtractor.State

fromSImm8 :: SImm8 -> Value
fromSImm8 s = fromBitsLE $ blastLE s ++ replicate 56 (sTestBit s 7)

fromSImm10 :: SImm10 -> InstructionAddress
fromSImm10 s = fromBitsLE $ (take 10 $ blastLE s) ++ replicate 6 (sTestBit s 9)

(<~) :: (c -> Machine ()) -> (Machine a, a -> b -> c, Machine b) -> Machine ()
(<~) res (arg1, op, arg2) = do
    x <- arg1
    y <- arg2
    res $ x `op` y

--------------------------------------------------------------------------------

execute :: Instruction -> Machine ()
execute (Halt             ) = writeFlag Halted true
execute (Ld    rX dmemaddr) = readMemory dmemaddr >>= writeRegister rX
execute (Ld_si rX simm    ) = (writeRegister rX $ fromSImm8 simm)
execute (St    rX dmemaddr) = readRegister rX >>= writeMemory dmemaddr
execute (Sub   rX dmemaddr) = do
    x <- readRegister rX
    y <- readMemory dmemaddr
    let z = x - y
    writeFlag Zero (z .== 0)
    writeRegister rX z
execute (Jmpi  simm       ) =
    modify $ \currentState ->
        currentState {instructionCounter =
            instructionCounter currentState + fromSImm10 simm}
execute (Jz simm          ) = do
    zeroIsSet <- readFlag Zero
    ic <- instructionCounter <$> get
    let ic' = ite zeroIsSet (ic + fromSImm10 simm) ic
    -- let ic' = ic
    modify $ \currentState ->
        currentState {instructionCounter = ic'}
    -- condition <- readFlag Zero
    -- state <- get
    -- let jumpState = snd $ run (jmpi simm) state
    -- put $ ite condition jumpState state

executeInstruction :: Machine ()
executeInstruction = do
    fetchInstruction
    incrementInstructionCounter
    execute =<< readInstructionRegister

fetchInstruction :: Machine ()
fetchInstruction =
    get >>= readProgram . instructionCounter >>= writeInstructionRegister

readProgram :: InstructionAddress -> Machine Instruction
readProgram addr = do
    currentState <- get
    delay 1
    -- let instr = case lookupInstruction addr (program currentState) of
    --                 Nothing -> Halt
    --                 Just i  -> i
    -- pure instr
    pure $ lookupInstruction addr (program currentState)
    where
        -- lookupInstruction :: InstructionAddress -> Program -> Maybe Instruction
        -- lookupInstruction _ [] = Nothing
        -- lookupInstruction addr ((k, v):rest) =
        --     ite (addr .== k) (Just v) (lookupInstruction addr rest)
        lookupInstruction :: InstructionAddress -> Program -> Instruction
        lookupInstruction _ [] = Jmpi 0
        lookupInstruction addr ((k, v):rest) =
            ite (addr .== k) v (lookupInstruction addr rest)

readInstructionRegister :: Machine Instruction
readInstructionRegister = instructionRegister <$> get

writeInstructionRegister :: Instruction -> Machine ()
writeInstructionRegister instruction =
    modify $ \currentState ->
        currentState {instructionRegister = instruction}

halt :: Machine ()
halt = writeFlag Halted true

ld :: Register -> MemoryAddress -> Machine ()
ld rX dmemaddr = readMemory dmemaddr >>= writeRegister rX

ld_si :: Register -> SImm8 -> Machine ()
ld_si rX simm = (writeRegister rX $ fromSImm8 simm)

st :: Register -> MemoryAddress -> Machine ()
st rX dmemaddr = readRegister rX >>= writeMemory dmemaddr

sub :: Register -> MemoryAddress -> Machine ()
sub rX dmemaddr = do
    x <- readRegister rX
    y <- readMemory dmemaddr
    let z = x - y
    -- iteLazy (z .== 0) (writeFlag Zero true) (writeFlag Zero false)
    writeFlag Zero (z .== 0)
    writeRegister rX z

jmpi :: SImm10 -> Machine ()
jmpi simm =
    modify $ \currentState ->
                currentState {instructionCounter =
                    instructionCounter currentState + fromSImm10 simm}

jz :: SImm10 -> Machine ()
jz simm = do
    zeroIsSet <- readFlag Zero
    ic <- instructionCounter <$> get
    let ic' = iteLazy zeroIsSet (ic + fromSImm10 simm) ic
    -- let ic' = ic
    modify $ \currentState ->
        currentState {instructionCounter = ic'}
    -- condition <- readFlag Zero
    -- state <- get
    -- let jumpState = snd $ run (jmpi simm) state
    -- put $ ite condition jumpState state
--------------------------------------------------------------------------------
