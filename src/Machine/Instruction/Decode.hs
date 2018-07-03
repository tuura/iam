module Machine.Instruction.Decode where

-- Decode the instruction AST from an enstruction code

import Data.Bits
import Machine.Types
import Machine.Instruction
import Machine.Value

decode :: (Num v, Eq v, IsRegister r, IsMemoryAddress addr, IsFlag flag)
       => v -> Instruction r addr flag byte
decode = undefined

type InstructionCode = Value

type Opcode = Value

type RegisterCode = Value

pad :: Int -> [Bool]
pad k = replicate k False

decodeOpcode :: InstructionCode -> Opcode
decodeOpcode c = fromBitsLE $ (drop 10 $ blastLE c) ++ pad 2

decodeRegister :: InstructionCode -> RegisterCode
decodeRegister c = fromBitsLE $ (take 2 $ drop 8 $ blastLE c) ++ pad 6

decodeMemoryAddress :: InstructionCode -> MemoryAddress
decodeMemoryAddress c = fromBitsLE $ (take 8 $ blastLE c)

decodeByte :: InstructionCode -> Byte
decodeByte c = fromBitsLE $ (take 8 $ blastLE c)
--------------------------------------------------------------------------------
blastLE :: (Num a, FiniteBits a) => a -> [Bool]
blastLE x = map (testBit x) [0 .. finiteBitSize x - 1]

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