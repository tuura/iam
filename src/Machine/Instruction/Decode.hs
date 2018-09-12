{-# LANGUAGE LambdaCase, MultiWayIf, TypeFamilies, FlexibleContexts #-}
module Machine.Instruction.Decode where

import Data.Bits
import Machine.Types
import Machine.Instruction
import Data.Word (Word16)
import Data.Maybe (fromJust)
import Data.Monoid ((<>))

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
                    (fromBitsLE $ extractSImm8 expandedCode)
          | opcode == [f, f, f, t, f, f]   ->
                Store (decodeRegister . extractRegister $ expandedCode)
                      (fromBitsLE $ extractMemoryAddress expandedCode)
          | opcode == [f, f, f, t, f, t]   ->
                Add (decodeRegister . extractRegister $ expandedCode)
                    (fromBitsLE $ extractMemoryAddress expandedCode)
          | opcode == [f, f, f, t, t, f]   ->
                Jump (fromBitsLE $ extractSImm8Jump expandedCode)
          | opcode == [f, f, f, t, t, t]    ->
                JumpZero (fromBitsLE $ extractSImm8Jump expandedCode)
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
                Mod (decodeRegister . extractRegister $ expandedCode)
                    (fromBitsLE $ extractMemoryAddress expandedCode)
          | opcode == [f, f, t, t, f, f]   ->
                Abs (decodeRegister . extractRegister $ expandedCode)
      where f = False
            t = True

decodeRegister :: [Bool] -> Register
decodeRegister = \case
      [False, False] -> R0
      [False, True]  -> R1
      [True, False]  -> R2
      [True, True]   -> R3
      _              -> error $ "Machine.Instruction.Decode.decodeRegister:"
                             <> "register must be encoded as a two-bit word"

decodeOpcode :: [Bool] -> [Bool]
decodeOpcode = take 6

extractRegister :: [Bool] -> [Bool]
extractRegister = take 2 . drop 6

extractMemoryAddress :: [Bool] -> [Bool]
extractMemoryAddress = (++ pad 56) . take 8 . drop 8

extractSImm8 :: [Bool] -> [Bool]
extractSImm8 = (++ pad 56) . take 8 . drop 8

extractSImm8Jump :: [Bool] -> [Bool]
extractSImm8Jump = (++ pad 56) . take 8 . drop 6

pad :: Int -> [Bool]
pad k = replicate k False
