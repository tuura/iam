{-# LANGUAGE LambdaCase #-}
module Machine.Instruction.Encode where

-- Decode the instruction AST from an enstruction code

import Data.Bits
import Machine.Types
import Machine.Instruction
import Machine.Value
import Data.Word (Word16)

type InstructionCode = Word16



encode :: (Instruction Register addr flag byte) -> InstructionCode
encode = \case
    Halt -> 0
    Load r addr -> fromBitsLE $ [f, f, f, f, f, f, f, t] ++ encodeRegister r
    where
        f = False
        t = True
-- -- | Get the instruction opcode (6 bits) and place to the
-- opcode :: Instruction -> InstructionCode

encodeRegister :: Register -> [Bool]
encodeRegister R0 = [False, False]
encodeRegister R1 = [False, True]
encodeRegister R2 = [True, False]
encodeRegister R3 = [True, True]

encodeMemoryAddress :: MemoryAddress -> [Bool]
encodeMemoryAddress = error "Not implemented"

encodeByte :: Byte -> [Bool]
encodeByte = error "Not emplemented"

--------------------------------------------------------------------------------
fromBitsLE :: (Num a, FiniteBits a) => [Bool] -> a
fromBitsLE bs
    | length bs /= w
        = error $ "IAM.fromBitsLE: Expected: " ++ show w ++ " bits, received: " ++ show (length bs)
    | True
        = result
    where w = finiteBitSize result
          result = go 0 0 bs

          go acc _  []    = acc
          go acc i (x:xs) = go (if x then (setBit acc i) else acc) (i+1) xs