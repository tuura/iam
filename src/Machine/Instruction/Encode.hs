{-# LANGUAGE LambdaCase, TypeFamilies, FlexibleContexts #-}
module Machine.Instruction.Encode where

-- Decode the instruction AST from an enstruction code

import Data.Bits
import Machine.Types
import Machine.Instruction
import Data.Word (Word16)
import qualified Data.SBV as SBV
import Data.Maybe (fromJust)
import Data.SBV (Boolean (..))

encode :: Instruction -> InstructionCode
encode = \case
    Halt -> 0
    Load     r addr -> fromBitsLE $ [f, f, f, f, f, t] ++ encodeRegister r
                                                       ++ encodeMemoryAddress addr
                                                       ++ pad 48
    LoadMI   r addr -> fromBitsLE $ [f, f, f, f, t, f] ++ encodeRegister r
                                                       ++ encodeMemoryAddress addr
                                                       ++ pad 48
    Set      r byte -> fromBitsLE $ [f, f, f, f, t, t] ++ encodeRegister r
                                                       ++ encodeByte byte
                                                       ++ pad 48
    Store    r addr -> fromBitsLE $ [f, f, f, t, f, f] ++ encodeRegister r
                                                       ++ encodeMemoryAddress addr
                                                       ++ pad 48
    Add      r addr -> fromBitsLE $ [f, f, f, t, f, t] ++ encodeRegister r
                                                       ++ encodeMemoryAddress addr
                                                       ++ pad 48
    Jump     byte   -> fromBitsLE $ [f, f, f, t, t, f] ++ encodeByte byte
                                                       ++ pad 50
    JumpZero byte   -> fromBitsLE $ [f, f, f, t, t, t] ++ encodeByte byte
                                                       ++ pad 50
    Sub      r addr -> fromBitsLE $ [f, f, t, f, f, f] ++ encodeRegister r
                                                       ++ encodeMemoryAddress addr
                                                       ++ pad 48
    Mul      r addr -> fromBitsLE $ [f, f, t, f, f, t] ++ encodeRegister r
                                                       ++ encodeMemoryAddress addr
                                                       ++ pad 48
    Div      r addr -> fromBitsLE $ [f, f, t, f, t, f] ++ encodeRegister r
                                                       ++ encodeMemoryAddress addr
                                                       ++ pad 48
    Mod      r addr -> fromBitsLE $ [f, f, t, f, t, t] ++ encodeRegister r
                                                       ++ encodeMemoryAddress addr
                                                       ++ pad 48
    Abs      r      -> fromBitsLE $ [f, f, t, t, f, f] ++ encodeRegister r
                                                       ++ pad 56
    where f = false
          t = true
          pad k = replicate k false

-- | 'Register' is encoded as a 2-bit word
encodeRegister :: Register -> [Bool]
encodeRegister = \case
    R0 -> [false, false]
    R1 -> [false, true]
    R2 -> [true, false]
    R3 -> [true, true]

-- | 'MemoryAddress' is stored in the leading 8 bits (little-endian) of a 'Value'
encodeMemoryAddress :: MemoryAddress -> [Bool]
encodeMemoryAddress = take 8 . blastLE

-- | 'Byte' is stored in the leading 8 bits (little-endian) of a 'Value'
encodeByte :: SImm8 -> [Bool]
encodeByte = take 8 . blastLE
--------------------------------------------------------------------------------
