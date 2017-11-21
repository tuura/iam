{-# LANGUAGE BinaryLiterals #-}

module Subtractor.Types where

import Data.Bits hiding (bit, xor)
import Data.SBV

-- | The 'Value' datatype represents data values. The precise
-- bit-width is left unspecified, but it is assumed that it fits into 64 bits.
type Value = SWord64

-- | The 'SImm8' datatype represents 8-bit signed immediate arguments that are
-- used by many Subtractor instructions with immediate addressing mode.
type SImm8 = SInt8

-- | The 'SImm10' datatype represents 10-bit signed immediate arguments that are
-- used for specifying the relative jump address, e.g. in
-- 'Subtractor.Assembly.Assembly.Jmpi' instruction.
type SImm10 = SInt16

-- | Subtractor has 4 general-purpose registers.
type Register = SWord8

-- | The register bank is represented by a map from registers to their values.
type RegisterBank = SFunArray Word8 Word64

-- | Subtractor memory can hold 256 values.
type MemoryAddress = SWord8

-- | The memory is represented by a map from memory addresses to their values.
type Memory = SFunArray Word8 Word64

-- type Program = [(InstructionAddress, Instruction)]

type Program = SFunArray Word16 Word16

-- | Programs are stored in program memory (currently, up to 1024 instructions).
type InstructionAddress = SWord16

-- | 'Opcode' is the leading 6-bit part of the 'InstructionCode', which
-- determines the instruction. The remaining 10 bits of the 'InstructionCode'
-- are used to specify immediate instruction arguments.
type Opcode = SWord8

-- | Instructions have 16-bit codes.
type InstructionCode = SWord16

-- | Boolean 'Flag's indicate the current status of Subtractor.
data Flag = Condition
          | Halted
          | Zero
          deriving (Enum, Eq, Ord, Show)

flagId :: Flag -> SWord8
flagId = literal . fromIntegral . fromEnum

-- | The state of flags is represented by a map from flags to their values.
type Flags = SFunArray Word8 Bool

-- | 'Clock' is the current time measured in clock cycles. It used to model the
-- effect of the 'Subtractor.Semantics.wait' instruction.
type Clock = SWord64

--------------------------------------------------------------------------------
data Instruction = Halt
                 | Ld    Register MemoryAddress
                 | Ld_si Register SImm8
                 | St    Register MemoryAddress
                 | Sub   Register MemoryAddress
                 | Jmpi  SImm10
                 | Jz    SImm10
    deriving (Show, Eq)

-- instance Mergeable Instruction where
--     symbolicMerge _ _ Halt Halt = Halt
--     symbolicMerge f t (Ld    rX1 dmemaddr1) (Ld    rX2 dmemaddr2) =
--         Ld (symbolicMerge f t rX1 rX2) (symbolicMerge f t dmemaddr1 dmemaddr2)
--     symbolicMerge f t (Ld_si rX1 simm1    ) (Ld_si rX2 simm2)     =
--         Ld_si (symbolicMerge f t rX1 rX2) (symbolicMerge f t simm1 simm2)
--     symbolicMerge f t (St    rX1 dmemaddr1) (St    rX2 dmemaddr2) =
--         St (symbolicMerge f t rX1 rX2) (symbolicMerge f t dmemaddr1 dmemaddr2)
--     symbolicMerge f t (Sub   rX1 dmemaddr1) (Sub   rX2 dmemaddr2) =
--         Sub (symbolicMerge f t rX1 rX2) (symbolicMerge f t dmemaddr1 dmemaddr2)
--     symbolicMerge f t (Jmpi  simm1        ) (Jmpi  simm2)         =
--         Jmpi (symbolicMerge f t simm1 simm2)
--     symbolicMerge f t (Jz  simm1          ) (Jz    simm2)         =
--         Jz (symbolicMerge f t simm1 simm2)
--     symbolicMerge _ _ a b =
--         error $ "Subtractor.Types: No least-upper-bound for " ++ show (a, b)

encode :: Instruction -> InstructionCode
encode (Halt)              = opcode 0b000000
encode (Ld    rX dmemaddr) = opcode 0b000001 .|. (register rX .|. address dmemaddr)
encode (Ld_si rX simm)     = opcode 0b000010 .|. (register rX .|. simm8 simm)
encode (St    rX dmemaddr) = opcode 0b000011 .|. (register rX .|. address dmemaddr)
encode (Sub   rX dmemaddr) = opcode 0b000100 .|. (register rX .|. address dmemaddr)
encode (Jmpi  simm)        = opcode 0b000101 .|. (simm10 simm)
encode (Jz    simm)        = opcode 0b000110 .|. (simm10 simm)

decode :: InstructionCode -> Instruction
decode c =
    case decodeOpcode c of
        0b000000 -> Halt
        0b000001 -> let rX = decodeRegister c
                        dmemaddr = decodeMemoryAddress c
                    in Ld rX dmemaddr
        0b000010 -> let rX = decodeRegister c
                        simm = decodeSImm8 c
                    in Ld_si rX simm
        0b000011 -> let rX = decodeRegister c
                        dmemaddr = decodeMemoryAddress c
                    in St rX dmemaddr
        0b000100 -> let rX = decodeRegister c
                        dmemaddr = decodeMemoryAddress c
                    in Sub rX dmemaddr
        0b000101 -> let simm = decodeSImm10 c
                    in Jmpi simm
        0b000110 -> let simm = decodeSImm10 c
                    in Jz simm
        _        -> Halt
--     let oc = decodeOpcode c
--     in ite (oc .== 0b000001)
--            (let rX = decodeRegister c
--                 dmemaddr = decodeMemoryAddress c
--             in Ld rX dmemaddr) $
--             ite (oc .== 0b000010)
--                 (let rX = decodeRegister c
--                      simm = decodeSImm8 c
--                  in Ld_si rX simm) $
--                  ite (oc .== 0b000011)
--                      (let rX = decodeRegister c
--                           dmemaddr = decodeMemoryAddress c
--                      in St rX dmemaddr) $
--                      ite (oc .== 0b000100)
--                          (let rX = decodeRegister c
--                               dmemaddr = decodeMemoryAddress c
--                          in Sub rX dmemaddr) $
--                          ite (oc .== 0b000101)
--                              (let simm = decodeSImm10 c
--                              in Jmpi simm) $
--                              ite (oc .== 0b000110)
--                                  (let simm = decodeSImm10 c
--                                  in Jz simm) $ Halt


pad :: Int -> [SBool]
pad k = replicate k false

opcode :: Opcode -> InstructionCode
opcode o = fromBitsLE $ pad 10 ++ (take 6 $ blastLE o)

register :: Register -> InstructionCode
register r = fromBitsLE $ pad 8 ++ (take 2 $ blastLE r) ++ pad 6

address :: MemoryAddress -> InstructionCode
address a = fromBitsLE $ blastLE a ++ pad 8

simm8 :: SImm8 -> InstructionCode
simm8 s = fromBitsLE $ blastLE s ++ pad 8

simm10 :: SImm10 -> InstructionCode
simm10 s = fromBitsLE $ (take 10 $ blastLE s) ++ pad 6

decodeOpcode :: InstructionCode -> Opcode
decodeOpcode c = fromBitsLE $ (drop 10 $ blastLE c) ++ pad 2

decodeRegister :: InstructionCode -> Register
decodeRegister c = fromBitsLE $ (take 2 $ drop 8 $ blastLE c) ++ pad 6

decodeMemoryAddress :: InstructionCode -> MemoryAddress
decodeMemoryAddress c = fromBitsLE $ (take 8 $ blastLE c)

decodeSImm8 :: InstructionCode -> SImm8
decodeSImm8 c = fromBitsLE $ (take 8 $ blastLE c)

decodeSImm10 :: InstructionCode -> SImm10
decodeSImm10 c = fromBitsLE $ (take 10 $ blastLE c) ++ pad 6

-- and   rX dmemaddr = write 0b000001 (register rX .|. address dmemaddr)
-- or    rX dmemaddr = write 0b000010 (register rX .|. address dmemaddr)
-- xor   rX dmemaddr = write 0b000011 (register rX .|. address dmemaddr)
-- add   rX dmemaddr = write 0b000100 (register rX .|. address dmemaddr)
-- sub   rX dmemaddr = write 0b000101 (register rX .|. address dmemaddr)
-- mul   rX dmemaddr = write 0b000110 (register rX .|. address dmemaddr)
-- div   rX dmemaddr = write 0b000111 (register rX .|. address dmemaddr)
-- ld    rX dmemaddr = write 0b001000 (register rX .|. address dmemaddr)
-- st    rX dmemaddr = write 0b001001 (register rX .|. address dmemaddr)
-- ldmi  rX dmemaddr = write 0b001010 (register rX .|. address dmemaddr)
-- stmi  rX dmemaddr = write 0b001011 (register rX .|. address dmemaddr)
-- cmpeq rX dmemaddr = write 0b010001 (register rX .|. address dmemaddr)
-- cmplt rX dmemaddr = write 0b010010 (register rX .|. address dmemaddr)
-- cmpgt rX dmemaddr = write 0b010011 (register rX .|. address dmemaddr)
-- sl    rX dmemaddr = write 0b011100 (register rX .|. address dmemaddr)
-- sr    rX dmemaddr = write 0b011101 (register rX .|. address dmemaddr)
-- sra   rX dmemaddr = write 0b011110 (register rX .|. address dmemaddr)
--------------------------------------------------------------------------------







