module Machine.Program where -- (
--     -- * Program type and program parser
--     Program,
--     parseProgram, readProgram
--     ) where

import Control.Selective
import Machine.Types
import Machine.Instruction
-- import Applications.ISA.Instruction.Encode
-- import Applications.ISA.Instruction.Decode

-- | The program is represented by a map from instruction addresses to
--   instructions.
type Program = [(InstructionAddress, Instruction)]

-- readProgram :: FilePath -> IO Program
-- readProgram = (fmap parseProgram) . readFile

-- -- | Quick-and-dirty program parser.
-- --   Comments start with the '#' character.
-- --   Blank lines are ignored.
-- parseProgram :: String -> Program
-- parseProgram src =
--     let instructions = map read . removeBlankLines . removeComments . lines $ src
--     in addInstructionAddresses instructions
--     where removeComments = map (takeWhile (/= '#'))
--           removeBlankLines = filter (not . null)
--           addInstructionAddresses = zip [0..]

-- showProgram :: Program -> String
-- showProgram prog = let is = map snd $ prog
--                    in unlines . map show $ is
