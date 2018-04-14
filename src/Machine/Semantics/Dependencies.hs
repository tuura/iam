{-# LANGUAGE ConstraintKinds,
             RankNTypes,
             ScopedTypeVariables,
             FlexibleContexts,
             FlexibleInstances #-}
module Machine.Semantics.Dependencies where

import Prelude hiding (read, readIO)
import Data.List (intersect)
import Control.Monad.Writer
import Data.Either (partitionEithers)
import Algebra.Graph hiding (graph)
import Algebra.Graph.Export.Dot
import qualified Data.Set as Set
import Data.Maybe (fromJust)
import Data.String (fromString)
import Data.Functor.Const

import Metalanguage
import Machine.Semantics
import Machine.Instruction

-- | Calculate data dependencies of a semantic computation
--   The computation must have only static dependencies, hence the
--   'Applicative' constraint. In case of presence of non-static dependecies
--   'Nothing' is returned.
dependencies :: Semantics Applicative k v a
             -> Maybe ([k], [k])
dependencies task =
    partitionEithers . getConst <$>
    task trackingRead trackingWrite
  where trackingRead  k    = Const [Left k]
        trackingWrite k fv = fv *> Const [Right k]

data OracleAnswer k = Concurrent
                    | ReadConflict [k]
                    | WriteConflict [k]
                    | ReadWriteConflict [k]
    deriving (Show, Eq)

-- | Find out if two computations are data dependent by matching their
--   static dependencies
concurrencyOracle :: Eq k =>
                    Semantics Applicative k v1 a
                 -> Semantics Applicative k v2 a
                 -> Maybe (OracleAnswer k)
concurrencyOracle s1 s2 = do
    (r1, w1) <- dependencies s1
    (r2, w2) <- dependencies s2
    let readConflicts      = intersect r1 r2
        writeConflicts     = intersect w1 w2
        readWriteConflicts = intersect (r1 ++ w1) (r2 ++ w2)
    pure $ case (readConflicts, writeConflicts, readWriteConflicts) of
        ([], [], [] ) -> Concurrent
        (rs, [], rws) | rs == rws -> ReadConflict rs
        ([], ws, rws) | ws == rws -> WriteConflict ws
        (_ , _ , rws) -> ReadWriteConflict rws

-- | Compute static data flow graph of an instruction. In case of supplying a
--   monadic, i.e. data-dependent instruction, 'Nothing' is returned.
--
-- Since no data requiring simulation is performed, the semantics metalanguage
-- terms are mocked: 'read' becomes 'const 0' is 'write' is simply ignored.
instructionGraph :: (InstructionAddress, Instruction)
                    -> Maybe (Graph (Either MachineKey (InstructionAddress, Instruction)))
instructionGraph instrInfo@(_, instr) = do
    (ins, outs) <- dependencies (semanticsA instr)
    pure $ overlay (star (Right instrInfo) (map Left outs))
                   (transpose $ star (Right instrInfo) (map Left ins))

-- | Compute static data flow graph of a program. In case of supplying a
--   monadic, i.e. data-dependent instruction, 'Nothing' is returned.
programDataGraph :: Program
                 -> Maybe (Graph (Either MachineKey (InstructionAddress, Instruction)))
programDataGraph p = foldl go (Just empty) (map instructionGraph p)
    where go _   Nothing  = Nothing
          go acc g        = overlay <$> acc <*> g

--------------------------------------------------------------------------------
-- | Serialise data flow graph as a .dot string
drawGraph :: Graph (Either MachineKey (InstructionAddress, Instruction)) -> String
drawGraph g = export style g
  where
    style = defaultStyleViaShow
        { vertexName = \v -> "v" ++ show (fromJust $ Set.lookupIndex v names)
        , vertexAttributes = \x -> case x of
            Left  k      -> [ "shape"  := "circle"
                            , "label"  := show k ]
            Right (a, i) -> [ "shape" := "record"
                            , "label" := instructionLabel a i ] }
    names = vertexSet g
    instructionLabel a i = fromString (show a <> "|" <> show i)

------------------------------------------
