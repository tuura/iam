{-# LANGUAGE LambdaCase, MultiWayIf, TypeFamilies, FlexibleContexts #-}
module Machine.Instruction.Decode where

import Data.SBV (SBV)
import Data.Bits
import Machine.Types
import Machine.Instruction
import Machine.Value
import Data.Word (Word16)
import qualified Data.SBV as SBV
import Data.Maybe (fromJust)

decode :: ( IsRegister r, Eq r
          , IsMemoryAddress addr, MachineBits addr
          , IsInstructionCode code, Num code
          , IsByte byte
          , code ~ addr, code ~ byte
          , IsBool (BoolType addr), Eq (BoolType addr)
          )
       => code -> Instruction r addr flag byte
decode code =
    let expandedCode = blastLE code
        opcode = take 6 expandedCode
    in if | opcode == [false, false, false, false, false, false] -> Halt
          | opcode == [false, false, false, false, false, true]  ->
                Load (decodeRegister . extractRegister $ expandedCode)
                     (fromBitsLE $ extractMemoryAddress expandedCode)
          | opcode == [false, false, false, false, true, false]   ->
                LoadMI (decodeRegister . extractRegister $ expandedCode)
                       (fromBitsLE $ extractMemoryAddress expandedCode)
          | opcode == [false, false, false, false, true, true]  ->
                Set (decodeRegister . extractRegister $ expandedCode)
                    (fromBitsLE $ extractByte expandedCode)
          | opcode == [false, false, false, true, false, false]   ->
                Store (decodeRegister . extractRegister $ expandedCode)
                      (fromBitsLE $ extractMemoryAddress expandedCode)
          | opcode == [false, false, false, true, false, true]   ->
                Add (decodeRegister . extractRegister $ expandedCode)
                    (fromBitsLE $ extractMemoryAddress expandedCode)
          | opcode == [false, false, false, true, true, false]   ->
                Jump (fromBitsLE $ extractByteJump expandedCode)
          | opcode == [false, false, false, true, true, true]    ->
                JumpZero (fromBitsLE $ extractByteJump expandedCode)

decodeRegister :: (IsRegister r, IsBool b, Eq b) => [b] -> r
decodeRegister code | code == [false, false] = r0
                    | code == [false, true]  = r1
                    | code == [true, false]  = r2
                    | code == [true, true]   = r3

decodeOpcode :: IsBool b => [b] -> [b]
decodeOpcode = take 6

extractRegister = take 2 . drop 6

extractMemoryAddress :: IsBool b => [b] -> [b]
extractMemoryAddress = (++ pad 56) . take 8 . drop 8

extractByte :: IsBool b => [b] -> [b]
extractByte = (++ pad 56) . take 8 . drop 8

extractByteJump :: IsBool b => [b] -> [b]
extractByteJump = (++ pad 56) . take 8 . drop 6

pad :: IsBool b => Int -> [b]
pad k = replicate k false
