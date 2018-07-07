module Machine.Instruction (
    -- * Instruction syntax and related types
    Instruction (..), InstructionAddress, InstructionCode
    ) where

import Data.Word
import Machine.Types

-- | Iam instructions
data Instruction r addr flag byte = Halt
                                  | Load     r addr
                                  | LoadMI   r addr
                                  | Set      r byte
                                  | Store    r addr
                                  | Add      r addr
                                  | Jump     byte
                                  | JumpZero byte
                                  | Sub      r addr
                                  | Mul      r addr
                                  | Div      r addr
                                  | Abs      r
    deriving (Show, Read, Eq, Ord)

-- | Programs are stored in program memory.
type InstructionAddress = Value

-- | Binary representation of an instruction
type InstructionCode = Value -- Word16

