module Machine.Semantics.Symbolic.SMT where

import qualified Data.Map.Strict as Map
import qualified Data.Tree as Tree
import qualified Data.Set as Set
import Data.Monoid
import Control.Monad.Reader (ask)
import Control.Monad.Trans (liftIO)
import qualified Data.SBV.Dynamic as SBV
import Data.SBV (constrain, SBool)

import Machine.Types
import Machine.Instruction.Decode
import Machine.Semantics.Symbolic.Types

type SValMap = Map.Map Int (SBV.Symbolic SBV.SVal)

-- | Walk the constraint gathering up the free
-- | variablesBV.
gatherFree :: Sym -> Set.Set Sym
gatherFree c@(SAny _) = Set.singleton c
gatherFree (SAdd l r) = gatherFree l <> gatherFree r
gatherFree (SSub l r) = gatherFree l <> gatherFree r
gatherFree (SDiv l r) = gatherFree l <> gatherFree r
gatherFree (SMod l r) = gatherFree l <> gatherFree r
gatherFree (SEq l r)  = gatherFree l <> gatherFree r
gatherFree (SAbs l)   = gatherFree l
gatherFree (SNot c)   = gatherFree c
gatherFree (SOr l r)  = gatherFree l <> gatherFree r
gatherFree (SAnd l r) = gatherFree l <> gatherFree r
gatherFree (SGt l r)  = gatherFree l <> gatherFree r
gatherFree (SLt l r)  = gatherFree l <> gatherFree r
gatherFree (SConst _)   = mempty

-- | Create an existential word of `i` bits with
-- | the name `name`.
sWordEx :: Int -> String -> SBV.Symbolic SBV.SVal
sWordEx i name =  ask >>= liftIO . SBV.svMkSymVar (Just SBV.EX) (SBV.KBounded True i) (Just name)

-- | Create existential SVals for each of CAny's in the input.
createSym :: [Sym] -> SBV.Symbolic (Map.Map Int SBV.SVal)
createSym cs = do
  pairs <- traverse createSymPair cs
  pure $  Map.fromList pairs
  where readableName i = valName $ i
        createSymPair (SAny i) = do
          v <- sWordEx 16 (readableName i)
          pure (i, v)
        createSymPair _ = error "Non-variable encountered."

-- | Convert a list of path constraints to a
-- | symbolic value the SMT solver can solve.
-- | Each constraint in the list is conjoined
-- | with the others.
toSMT :: [Sym] -> SBV.Symbolic SBV.SVal
toSMT c = do
  let freeVars = gatherFree (foldr SAnd (SConst 1) c)
  sValMap <- createSym (Set.toList freeVars)
  smts <- traverse (symToSMT sValMap) c
  pure $ conjoin smts

symToSMT :: Map.Map Int SBV.SVal -> Sym -> SBV.Symbolic SBV.SVal
symToSMT m (SEq l r) =
  sValToSWord <$> (SBV.svEqual <$> symToSMT m l <*> symToSMT m r)
symToSMT m (SGt l r) =
  sValToSWord <$> (SBV.svGreaterThan  <$> symToSMT m l <*> symToSMT m r)
symToSMT m (SLt l r) =
  sValToSWord <$> (SBV.svLessThan <$> symToSMT m l <*> symToSMT m r)
symToSMT m (SAdd l r) =
  SBV.svPlus <$> symToSMT m l <*> symToSMT m r
symToSMT m (SSub l r) =
  SBV.svMinus <$> symToSMT m l <*> symToSMT m r
symToSMT m (SDiv l r) =
  SBV.svQuot <$> symToSMT m l <*> symToSMT m r
symToSMT m (SMod l r) =
  SBV.svRem <$> symToSMT m l <*> symToSMT m r
symToSMT _ (SConst w) =  pure $ valueToSVal w
symToSMT m (SAbs l) =
  SBV.svAbs <$> symToSMT m l
symToSMT m (SNot c) =
  let c' = symToSMT m c
  in sValToSWord <$> (SBV.svNot <$> (sValToSBool <$> c'))
symToSMT m (SAnd l r) =
  let l' = sValToSBool <$> symToSMT m l
      r' = sValToSBool <$> symToSMT m r
  in sValToSWord <$> (SBV.svAnd <$> l' <*> r')
symToSMT m (SOr l r) =
  let l' = sValToSBool <$> symToSMT m l
      r' = sValToSBool <$> symToSMT m r
  in sValToSWord <$> (SBV.svOr <$> l' <*> r')
symToSMT m (SAny i) = do
  case Map.lookup i m of
    Just val -> pure val
    Nothing -> error "Missing symbolic variable."

valueToSVal :: Value -> SBV.SVal
valueToSVal w = SBV.svInteger (SBV.KBounded True 16) (toInteger w)

sValToSBool :: SBV.SVal -> SBV.SVal
sValToSBool w = w `SBV.svNotEqual` (valueToSVal 0)

sValToSWord :: SBV.SVal -> SBV.SVal
sValToSWord w = SBV.svIte w (valueToSVal 1) (valueToSVal 0)

renderSMTResult :: SBV.SMTResult -> String
renderSMTResult (SBV.Unsatisfiable _ _) = "Unsatisfiable"
renderSMTResult s@(SBV.Satisfiable _ _) =
  let dict = SBV.getModelDictionary s
  in
    if Map.null dict then "Trivial" else renderDict dict
renderSMTResult _ = "Error"

renderSolvedState :: SolvedState -> String
renderSolvedState (SolvedState state c) =
  "IC: " <> show (instructionCounter state) <> "\n" <>
  "IR: " <> show (decode $ instructionRegister state) <> "\n" <>
  "Flags: " <> show (Map.toList $ flags state) <> "\n" <>
--   "Stack: " <> show (renderSym <$> st) <> "\n" <>
  "Path Constraints: " <> show (foldr SAnd (SConst 1) (pathConstraintList state)) <> "\n" <>
  "Solved Values: " <> renderSMTResult c

renderDict :: (Show v) => Map.Map String v -> String
renderDict m =
  foldr toStr "" (Map.toList m)
  where toStr (k,v) s = k <> " = " <> show v <> ", " <> s

data SolvedState = SolvedState SymState SBV.SMTResult

solveSym :: Trace -> IO (Tree.Tree SolvedState)
solveSym (Tree.Node state c) = do
    let smtExpr = toSMT (pathConstraintList state)
    SBV.SatResult smtRes <- SBV.satWith prover (smtExpr)
    children <- traverse solveSym c
    pure $ Tree.Node (SolvedState state smtRes) children

conjoin :: [SBV.SVal] -> SBV.SVal
conjoin = foldr (SBV.svAnd . sValToSBool) SBV.svTrue

valName :: Int -> String
valName i = "val_" <> (show i)

prover :: SBV.SMTConfig
prover = SBV.z3 { SBV.verbose = True
                , SBV.redirectVerbose = Just "log.smt2"
                , SBV.printBase = 10
                }