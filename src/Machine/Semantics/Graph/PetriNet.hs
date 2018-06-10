{-# LANGUAGE RankNTypes, TupleSections #-}
module Machine.Semantics.Graph.PetriNet where

import Control.Monad.State
import Control.Monad.Writer
import qualified Data.Map as Map
import Algebra.Graph hiding (graph)
import Algebra.Graph.Export.Dot
import qualified Data.Set as Set
import Data.Maybe (fromJust)
import Data.String (fromString)
import Text.Read (read)
import System.Process (callCommand)

import Machine.Semantics
import Machine.Instruction
import Machine.Semantics.Dependencies

-- | A relative position in time.
type Timestamp = Int

-- | An unfolded Petri Net simulating program's behaviour.
-- | Each modification of the resource produces a new place.
type PetriNet = Graph (Either (MachineKey, Timestamp)
                              (InstructionAddress, Instruction)
                      )

-- | Increment the timestamp associated with a given key.
incrementTimestamp :: MachineKey -> State (Map.Map MachineKey Timestamp) ()
incrementTimestamp key = do
    current <- get
    put $ Map.alter f key current
    where f (Just v) = Just $ v + 1
          f Nothing  = Just 0

-- | Construct the unfolded Petri Net model of a single instruction
-- | (opcode with arguments)
instructionPN :: (InstructionAddress, Instruction)
               -> State (Map.Map MachineKey Timestamp) PetriNet
instructionPN instrInfo@(i, instr) = do
    let (Just (ins, outs)) = dependencies (semanticsA instr)
    ins' <- matchDeps <$> get <*> pure ins
    mapM incrementTimestamp outs
    outs'  <- matchDeps <$> get <*> pure outs
    pure $ overlay (star (Right instrInfo) (map Left outs'))
                   (transpose $ star (Right instrInfo) (map Left ins'))
    where
        matchDeps :: Map.Map MachineKey Timestamp
                  -> [MachineKey]
                  -> [(MachineKey, Timestamp)]
        matchDeps globalKeys localKeys =
            foldr (\(x, ts) acc -> if x `elem` localKeys
                                   then (x, ts):acc
                                   else acc) [] (Map.toList globalKeys)

-- | Construct the unfolded Petri Net model of a program.
programPN :: Program -> PetriNet
programPN p =
    let (ins, outs) = fromJust $ dependencies (blockSemanticsA $ map snd p)
        initKeys = Map.fromList . map (, 0) $ ins ++ outs
    in (flip evalState) initKeys $
        foldM (\acc i -> overlay <$> pure acc <*> instructionPN i) empty p

drawPN :: PetriNet -> String
drawPN g = export style g
  where
    style = defaultStyleViaShow
        { vertexName = \v -> "v" ++ show (fromJust $ Set.lookupIndex v names)
        , vertexAttributes = \x -> case x of
            Left  (i, k)      -> [ "shape"  := "circle"
                            , "label"  := (show i ++ " | " ++ show k) ]
            Right (a, i) -> [ "shape" := "record"
                            , "label" := instructionLabel a i ] }
    names = vertexSet g
    instructionLabel a i = fromString (show a <> "|" <> show i)

readProgram :: FilePath -> IO Program
readProgram = (fmap parseProgram) . readFile

parseProgram :: String -> Program
parseProgram = addInstructionAddresses . map read
             . removeBlankLines . removeComments . lines
    where removeComments = map (takeWhile (/= '#'))
          removeBlankLines = filter (not . null)
          addInstructionAddresses = zip [0..]

writeSvgProgramDataGraph :: FilePath -> FilePath -> IO ()
writeSvgProgramDataGraph sourceCode svgFileName = do
    src <- readProgram sourceCode
    let graph = programPN $ src
        dotFileName = Prelude.takeWhile (/= '.') svgFileName ++ ".dot"
    writeFile dotFileName (drawPN graph)
    callCommand ("dot -Tsvg " ++ dotFileName ++ " -o " ++ svgFileName)