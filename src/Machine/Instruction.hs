module Machine.Instruction (
    -- * Instruction syntax and related types
    Instruction (..), Opcode, InstructionAddress,

    -- * Program type and program parser
    Program,
    parseProgram, readProgram
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

                 | AdjustVelocity Register MemoryAddress
                 | CheckOperationStatus Register MemoryAddress MemoryAddress
    deriving (Show, Read, Eq, Ord)

type Opcode = Word8

-- | Programs are stored in program memory.
type InstructionAddress = Value

-- | The program is represented by a map from instruction addresses to
--   instructions.
type Program = [(InstructionAddress, Instruction)]

readProgram :: FilePath -> IO Program
readProgram = (fmap parseProgram) . readFile

-- | Quick-and-dirty program parser.
--
--   Comments start with the '#' character.
--
--   Blank lines are ignored.
parseProgram :: String -> Program
parseProgram = addInstructionAddresses . map read
             . removeBlankLines . removeComments . lines
    where removeComments = map (takeWhile (/= '#'))
          removeBlankLines = filter (not . null)
          addInstructionAddresses = zip [0..]
