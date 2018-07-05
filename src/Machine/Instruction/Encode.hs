{-# LANGUAGE LambdaCase, TypeFamilies, FlexibleContexts #-}
module Machine.Instruction.Encode where

-- Decode the instruction AST from an enstruction code

import Data.Bits
import Machine.Types
import Machine.Instruction
import Machine.Value
import Data.Word (Word16)
import qualified Data.SBV as SBV
import Data.Maybe (fromJust)

encode :: ( IsRegister r, Eq r
          , IsMemoryAddress addr, MachineBits addr
          , Num code
          , IsByte byte
          , code ~ addr, code ~ byte
          , IsBool (BoolType addr)
          )
       => Instruction r addr flag byte -> code
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
    where f = false
          t = true
          pad k = replicate k false

-- | 'Register' is encoded as a 2-bit word
encodeRegister :: (Eq r, IsRegister r, IsBool b) => r -> [b]
encodeRegister r | r == r0 = [false, false]
                 | r == r1 = [false, true]
                 | r == r2 = [true, false]
                 | r == r3 = [true, true]

-- | 'MemoryAddress' is stored in the leading 8 bits (little-endian) of a 'Value'
encodeMemoryAddress :: (IsMemoryAddress addr, MachineBits addr) => addr -> [BoolType addr]
encodeMemoryAddress = take 8 . blastLE

-- | 'Byte' is stored in the leading 8 bits (little-endian) of a 'Value'
encodeByte :: (IsByte b, MachineBits b) => b -> [BoolType b]
encodeByte = take 8 . blastLE
--------------------------------------------------------------------------------
