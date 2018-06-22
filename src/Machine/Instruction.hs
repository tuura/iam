module Machine.Instruction (
    -- * Instruction syntax and related types
    Instruction (..), InstructionAddress,

    -- * Program type and program parser
    Program,
    parseProgram, readProgram
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
    deriving (Show, Read, Eq, Ord)

-- | Programs are stored in program memory.
type InstructionAddress = Value

-- | The program is represented by a map from instruction addresses to
--   instructions.
type Program = [(InstructionAddress, Instruction Register MemoryAddress Flag Byte)]

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
