module Machine.Instruction where

import Data.Word
import Data.Bits
import Machine.Types

-- | Iam instructions
data Instruction = Halt
                 | Load     Register MemoryAddress
                 | LoadMI   Register MemoryAddress
                 | Set      Register SImm8
                 | Store    Register MemoryAddress
                 | Add      Register MemoryAddress
                 | Jump     SImm10
                 | JumpZero SImm10

                 | AdjustVelocity Register MemoryAddress
                 | CheckOperationStatus Register MemoryAddress MemoryAddress
    deriving (Show, Read, Eq, Ord)

type Opcode = Word8

type InstructionCode = Word64

-- | Programs are stored in program memory.
type InstructionAddress = Value

-- | The program is represented by a map from instruction addresses to
--   instructions.
type Program = [(InstructionAddress, Instruction)]

readProgram :: FilePath -> IO Program
readProgram = (fmap parseProgram) . readFile

parseProgram :: String -> Program
parseProgram = addInstructionAddresses . map read
             . removeBlankLines . removeComments . lines
    where removeComments = map (takeWhile (/= '#'))
          removeBlankLines = filter (not . null)
          addInstructionAddresses = zip [0..]
