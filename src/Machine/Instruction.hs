module Machine.Instruction where

import Data.Word
import Data.Bits
import Machine.Types

-- | Iam instructions
data Instruction = Halt
                 | Load     Register MemoryAddress
                 | LoadMI   Register MemoryAddress
                 | Set      Register SImm8
                 | Store    Register MemoryAddress
                 | Add      Register MemoryAddress
                 | Jump     SImm10
                 | JumpZero SImm10
    deriving (Show, Eq, Ord)

type Opcode = Word8

type InstructionCode = Word64

-- | Programs are stored in program memory.
type InstructionAddress = Value

-- | The program is represented by a map from instruction addresses to
--   instructions.
type Program = [(InstructionAddress, Instruction)]

--------------------------------------------------------------------------------
pad :: Int -> [Bool]
pad k = replicate k False

decodeOpcode :: InstructionCode -> Opcode
decodeOpcode c = fromBitsLE $ (drop 10 $ blastLE c) ++ pad 2

-- decodeRegister :: InstructionCode -> Register
-- decodeRegister c = fromBitsLE $ (take 2 $ drop 8 $ blastLE c) ++ pad 6

decodeMemoryAddress :: InstructionCode -> MemoryAddress
decodeMemoryAddress c = fromBitsLE $ (take 8 $ blastLE c)

decodeSImm8 :: InstructionCode -> SImm8
decodeSImm8 c = fromBitsLE $ (take 8 $ blastLE c)

decodeSImm10 :: InstructionCode -> SImm10
decodeSImm10 c = fromBitsLE $ (take 10 $ blastLE c) ++ pad 6

blastLE :: FiniteBits a => a -> [Bool]
blastLE x = map (testBit x) [0..(finiteBitSize x-1)]

fromBitsLE :: (Num a, FiniteBits a) => [Bool] -> a
fromBitsLE bs = go 0 0 bs
    where go acc _ []     = acc
          go acc i (x:xs) = go (if x then (setBit acc i) else acc) (i+1) xs


-- decodeUImm8 :: InstructionCode -> UImm8
-- decodeUImm8 c = fromBitsLE $ (take 8 $ blastLE c)

-- decodeUImm10 :: InstructionCode -> UImm10
-- decodeUImm10 c = fromBitsLE $ (take 10 $ blastLE c) ++ pad 6