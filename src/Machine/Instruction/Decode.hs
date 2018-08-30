{-# LANGUAGE LambdaCase, MultiWayIf, TypeFamilies, FlexibleContexts #-}
module Machine.Instruction.Decode where

import Data.Bits
import Machine.Types
import Machine.Instruction
import Data.Word (Word16)
import Data.Maybe (fromJust)

decode :: InstructionCode -> Instruction
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
      where f = False
            t = True

decodeRegister :: [Bool] -> Register
decodeRegister code | code == [False, False] = R0
                    | code == [False, True]  = R1
                    | code == [True, False]  = R2
                    | code == [True, True]   = R3

decodeOpcode :: [Bool] -> [Bool]
decodeOpcode = take 6

extractRegister = take 2 . drop 6

extractMemoryAddress :: [Bool] -> [Bool]
extractMemoryAddress = (++ pad 56) . take 8 . drop 8

extractByte :: [Bool] -> [Bool]
extractByte = (++ pad 56) . take 8 . drop 8

extractByteJump :: [Bool] -> [Bool]
extractByteJump = (++ pad 56) . take 8 . drop 6

pad :: Int -> [Bool]
pad k = replicate k False
