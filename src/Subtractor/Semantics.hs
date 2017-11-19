module Subtractor.Semantics where

import Data.SBV
import Control.Monad.State.Strict (modify, runState, get)
import Subtractor.Types
import Subtractor.Verification

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
execute (Halt             ) = writeFlag Halted true
execute (Ld    rX dmemaddr) = readMemory dmemaddr >>= writeRegister rX
execute (Ld_si rX simm    ) = (writeRegister rX $ fromSImm8 simm)
execute (St    rX dmemaddr) = readRegister rX >>= writeMemory dmemaddr
execute (Sub   rX dmemaddr) =
    writeRegister rX <~ (readRegister rX, (-), readMemory dmemaddr)
execute (Jmpi  simm       ) =
    modify $ \currentState ->
        currentState {instructionCounter =
            instructionCounter currentState + fromSImm10 simm}

verify :: Int -> MachineState -> MachineState
verify steps state
    | steps == 0 = state
    | otherwise  = ite halted state (verify (steps - 1) nextState)
  where
    halted    = readArray (flags state) (flagId Halted)
    nextState = snd $ run executeInstruction state

executeInstruction :: Machine ()
executeInstruction = do
    fetchInstruction
    incrementInstructionCounter
    execute =<< readInstructionRegister

fetchInstruction :: Machine ()
fetchInstruction =
    get >>= readProgram . instructionCounter >>= writeInstructionRegister

readProgram :: InstructionAddress -> Machine Instruction
readProgram address = do
    currentState <- get
    delay 1
    let instr = lookup address (program currentState)
    case instr of
        Nothing -> pure Halt
        Just i ->  pure i

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
sub rX dmemaddr =
    writeRegister rX <~ (readRegister rX, (-), readMemory dmemaddr)

jmpi :: SImm10 -> Machine ()
jmpi simm =
    modify $ \currentState ->
                currentState {instructionCounter =
                    instructionCounter currentState + fromSImm10 simm}

--------------------------------------------------------------------------------
