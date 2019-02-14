module Machine.Graph (
        instructionGraph, programGraph, drawGraph
    ) where

import qualified Data.Set as Set
import Data.Maybe (fromJust)
import Data.String (fromString)
import Algebra.Graph hiding (graph)
import Algebra.Graph.Export.Dot
import FS
import Machine
import Machine.Instruction
import Machine.Program

-- | Compute static data flow graph of an instruction.
instructionGraph :: (InstructionAddress, Instruction)
                    -> Graph (Either String (InstructionAddress, Instruction))
instructionGraph instrInfo@(_, instr) =
    let (ins, outs) = FS.dependencies (instructionSemantics' instr)
    in overlay (star (Right instrInfo) (map Left outs))
               (transpose $ star (Right instrInfo) (map Left ins))

-- | Compute static data flow graph of a program.
programGraph :: Program
                 -> Graph (Either String (InstructionAddress, Instruction))
programGraph p = foldl go empty (map instructionGraph p)
    where go acc g = overlay acc g
--------------------------------------------------------------------------------

-- | Serialise data flow graph as a .dot string
-- drawGraph :: Graph (Either String (InstructionAddress, Instruction)) -> String
drawGraph :: Graph (Either String (InstructionAddress, Instruction)) -> String
drawGraph g =
    let g' = stringifyVertex <$> g
        names = vertexSet g'
        style = defaultStyleViaShow
            { vertexName = \v -> "v" ++ show (fromJust $ Set.lookupIndex v names)
            , vertexAttributes = \x -> case x of
                Left  k -> [ "shape"  := "circle"
                           , "label"  := k ]
                Right i -> [ "shape" := "record"
                           , "label" := i ] }
    in export style g'
    where
        stringifyVertex :: Either String (InstructionAddress, Instruction) ->
                    Either String String
        stringifyVertex (Left keyName) = Left  keyName
        stringifyVertex (Right (a, i)) = Right $ show a <> "|" <> show i

