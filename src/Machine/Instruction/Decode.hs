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
          , Num code
          , IsByte byte
          , code ~ addr, code ~ byte
          , IsBool (BoolType addr), Eq (BoolType addr)
          )
       => code -> Instruction r addr flag byte
decode code =
    let expandedCode = blastLE code
        opcode = take 6 expandedCode
    in if | opcode == [f, f, f, f, f, f] -> Halt
          | opcode == [f, f, f, f, f, t]  ->
                Load (decodeRegister . extractRegister $ expandedCode)
                     (fromBitsLE $ extractMemoryAddress expandedCode)
          | opcode == [f, f, f, f, t, f]   ->
                LoadMI (decodeRegister . extractRegister $ expandedCode)
                       (fromBitsLE $ extractMemoryAddress expandedCode)
          | opcode == [f, f, f, f, t, t]  ->
                Set (decodeRegister . extractRegister $ expandedCode)
                    (fromBitsLE $ extractByte expandedCode)
          | opcode == [f, f, f, t, f, f]   ->
                Store (decodeRegister . extractRegister $ expandedCode)
                      (fromBitsLE $ extractMemoryAddress expandedCode)
          | opcode == [f, f, f, t, f, t]   ->
                Add (decodeRegister . extractRegister $ expandedCode)
                    (fromBitsLE $ extractMemoryAddress expandedCode)
          | opcode == [f, f, f, t, t, f]   ->
                Jump (fromBitsLE $ extractByteJump expandedCode)
          | opcode == [f, f, f, t, t, t]    ->
                JumpZero (fromBitsLE $ extractByteJump expandedCode)
          | opcode == [f, f, t, f, f, f]   ->
                Sub (decodeRegister . extractRegister $ expandedCode)
                    (fromBitsLE $ extractMemoryAddress expandedCode)
          | opcode == [f, f, t, f, f, t]   ->
                Mul (decodeRegister . extractRegister $ expandedCode)
                    (fromBitsLE $ extractMemoryAddress expandedCode)
          | opcode == [f, f, t, f, t, f]   ->
                Div (decodeRegister . extractRegister $ expandedCode)
                    (fromBitsLE $ extractMemoryAddress expandedCode)
          | opcode == [f, f, t, f, t, t]   ->
                Abs (decodeRegister . extractRegister $ expandedCode)
      where f = false
            t = true

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
