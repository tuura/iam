module Machine.Instruction (
    -- * Instruction syntax and related types
    Instruction (..), InstructionAddress, InstructionCode
    ) where

import Data.Word
import Machine.Types

-- | Iam instructions
data Instruction = Halt
                 | Load     Register MemoryAddress
                 | LoadMI   Register MemoryAddress
                 | Set      Register SImm8
                 | Store    Register MemoryAddress
                 | Add      Register MemoryAddress
                 | Jump     SImm8
                 | JumpZero SImm8
                 | Sub      Register MemoryAddress
                 | Mul      Register MemoryAddress
                 | Div      Register MemoryAddress
                 | Mod      Register MemoryAddress
                 | Abs      Register
    deriving (Show, Read, Eq, Ord)

-- | Programs are stored in program memory.
type InstructionAddress = Value

-- | Binary representation of an instruction
type InstructionCode = Value -- Word16

