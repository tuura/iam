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

-- execute :: Instruction -> Machine ()
-- execute (Halt             ) = writeFlag Halted true
-- execute (Ld    rX dmemaddr) = readMemory dmemaddr >>= writeRegister rX
-- execute (Ld_si rX simm    ) = (writeRegister rX $ fromSImm8 simm)
-- execute (St    rX dmemaddr) = readRegister rX >>= writeMemory dmemaddr
-- execute (Sub   rX dmemaddr) = do
--     x <- readRegister rX
--     y <- readMemory dmemaddr
--     let z = x - y
--     writeFlag Zero (z .== 0)
--     writeRegister rX z
-- execute (Jmpi  simm       ) =
--     modify $ \currentState ->
--         currentState {instructionCounter =
--             instructionCounter currentState + fromSImm10 simm}
-- execute (Jz simm          ) = do
--     zeroIsSet <- readFlag Zero
--     ic <- instructionCounter <$> get
--     let ic' = ite zeroIsSet (ic + fromSImm10 simm) ic
--     -- let ic' = ic
--     modify $ \currentState ->
--         currentState {instructionCounter = ic'}
--     -- condition <- readFlag Zero
--     -- state <- get
--     -- let jumpState = snd $ run (jmpi simm) state
--     -- put $ ite condition jumpState state

-- execute :: InstructionCode -> Machine ()
-- execute c =
--     case decodeOpcode c of
--         0b000000 -> halt
--         0b000001 -> let rX = decodeRegister c
--                         dmemaddr = decodeMemoryAddress c
--                     in ld rX dmemaddr
--         0b000010 -> let rX = decodeRegister c
--                         simm = decodeSImm8 c
--                     in ld_si rX simm
--         0b000011 -> let rX = decodeRegister c
--                         dmemaddr = decodeMemoryAddress c
--                     in st rX dmemaddr
--         0b000100 -> let rX = decodeRegister c
--                         dmemaddr = decodeMemoryAddress c
--                     in sub rX dmemaddr
--         0b000101 -> let simm = decodeSImm10 c
--                     in jmpi simm
--         0b000110 -> let simm = decodeSImm10 c
--                     in jz simm
--         _        -> halt
execute :: InstructionCode -> MachineState -> MachineState
execute c currentState =
    -- case decodeOpcode c of
    --     0b000000 -> snd $ run halt currentState
    --     0b000001 -> let rX = decodeRegister c
    --                     dmemaddr = decodeMemoryAddress c
    --                 in snd $ run (ld rX dmemaddr) currentState
    --     0b000010 -> let rX = decodeRegister c
    --                     simm = decodeSImm8 c
    --                 in snd $ run (ld_si rX simm) currentState
    --     0b000011 -> let rX = decodeRegister c
    --                     dmemaddr = decodeMemoryAddress c
    --                 in snd $ run (st rX dmemaddr) currentState
    --     0b000100 -> let rX = decodeRegister c
    --                     dmemaddr = decodeMemoryAddress c
    --                 in snd $ run (sub rX dmemaddr) currentState
    --     0b000101 -> let simm = decodeSImm10 c
    --                 in snd $ run (jmpi simm) currentState
    --     0b000110 -> let simm = decodeSImm10 c
    --                 in snd $ run (jz simm) currentState
    --     _        -> snd $ run halt currentState
    let oc = decodeOpcode c
    in ite (oc .== 0b000001)
           (let rX = decodeRegister c
                dmemaddr = decodeMemoryAddress c
            in snd $ run (ld rX dmemaddr) currentState) $
            ite (oc .== 0b000010)
                (let rX = decodeRegister c
                     simm = decodeSImm8 c
                 in snd $ run (ld_si rX simm) currentState) $
                 ite (oc .== 0b000011)
                     (let rX = decodeRegister c
                          dmemaddr = decodeMemoryAddress c
                     in snd $ run (st rX dmemaddr) currentState) $
                     ite (oc .== 0b000100)
                         (let rX = decodeRegister c
                              dmemaddr = decodeMemoryAddress c
                         in snd $ run (sub rX dmemaddr) currentState) $
                         ite (oc .== 0b000101)
                             (let simm = decodeSImm10 c
                             in snd $ run (jmpi simm) currentState) $
                             ite (oc .== 0b000110)
                                 (let simm = decodeSImm10 c
                                 in snd $ run (jz simm) currentState) $ snd $ run (halt) currentState

executeInstruction :: Machine ()
executeInstruction = do
    fetchInstruction
    incrementInstructionCounter
    (modify . execute) =<< readInstructionRegister

fetchInstruction :: Machine ()
fetchInstruction =
    get >>= readProgram . instructionCounter >>= writeInstructionRegister

readProgram :: InstructionAddress -> Machine InstructionCode -- Instruction
readProgram addr = do
    currentState <- get
    delay 1
    -- pure $ Jmpi 0
    pure $ readArray (program currentState) addr

readInstructionRegister :: Machine InstructionCode -- Instruction
readInstructionRegister = instructionRegister <$> get

writeInstructionRegister :: InstructionCode -> Machine ()
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
    let ic' = ite zeroIsSet (ic + fromSImm10 simm) ic
    -- let ic' = ic
    modify $ \currentState ->
        currentState {instructionCounter = ic'}
    -- condition <- readFlag Zero
    -- state <- get
    -- let jumpState = snd $ run (jmpi simm) state
    -- put $ ite condition jumpState state
--------------------------------------------------------------------------------
