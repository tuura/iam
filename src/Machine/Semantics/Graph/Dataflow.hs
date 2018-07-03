{-# LANGUAGE RankNTypes #-}
module Machine.Semantics.Graph.Dataflow where

import Control.Monad.Writer
import Algebra.Graph hiding (graph)
import Algebra.Graph.Export.Dot
import qualified Data.Set as Set
import Data.Maybe (fromJust)
import Data.String (fromString)

import Machine.Instruction
import Machine.Semantics
import Machine.Semantics.Dependencies

-- -- | Compute static data flow graph of an instruction. In case of supplying a
-- --   monadic, i.e. data-dependent instruction, 'Nothing' is returned.
-- --
-- -- Since no data requiring simulation is performed, the semantics metalanguage
-- -- terms are mocked: 'read' becomes 'const 0' is 'write' is simply ignored.
-- instructionGraph :: (InstructionAddress, Instruction)
--                     -> Maybe (Graph (Either MachineKey (InstructionAddress, Instruction)))
-- instructionGraph instrInfo@(_, instr) = do
--     (ins, outs) <- dependencies (semanticsA instr)
--     pure $ overlay (star (Right instrInfo) (map Left outs))
--                    (transpose $ star (Right instrInfo) (map Left ins))

-- -- | Compute static data flow graph of a program. In case of supplying a
-- --   monadic, i.e. data-dependent instruction, 'Nothing' is returned.
-- programDataGraph :: Program
--                  -> Maybe (Graph (Either MachineKey (InstructionAddress, Instruction)))
-- programDataGraph p = foldl go (Just empty) (map instructionGraph p)
--     where go _   Nothing  = Nothing
--           go acc g        = overlay <$> acc <*> g
-- --------------------------------------------------------------------------------
-- -- | Serialise data flow graph as a .dot string
-- drawGraph :: Graph (Either MachineKey (InstructionAddress, Instruction)) -> String
-- drawGraph g = export style g
--   where
--     style = defaultStyleViaShow
--         { vertexName = \v -> "v" ++ show (fromJust $ Set.lookupIndex v names)
--         , vertexAttributes = \x -> case x of
--             Left  k      -> [ "shape"  := "circle"
--                             , "label"  := show k ]
--             Right (a, i) -> [ "shape" := "record"
--                             , "label" := instructionLabel a i ] }
--     names = vertexSet g
--     instructionLabel a i = fromString (show a <> "|" <> show i)
