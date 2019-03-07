module Machine.Semantics.Symbolic.Types where

import qualified Data.Tree as Tree
import qualified Data.Map.Strict as Map
import Data.Monoid ((<>))
import Text.Pretty.Simple
import Control.Monad.State
import Control.Monad.List
import Machine.Types
import Machine.Instruction
import Machine.Instruction.Decode
import Machine.Instruction.Encode
import Machine.Program hiding (readProgram)
import Data.Word (Word8)

-- | Symbolic expressions
data Sym = SAdd Sym Sym
         | SSub Sym Sym
         | SDiv Sym Sym
         | SMod Sym Sym
         | SAbs Sym
         | SConst Value
         | SAnd Sym Sym
         | SOr Sym Sym
         | SAny Int     -- Any value or the set of all values
         | SEq Sym Sym
         | SGt Sym Sym
         | SLt Sym Sym
         | SNot Sym
         | SIte Sym Sym Sym
         deriving (Eq, Ord)

mergeSym :: Sym -> Sym -> Sym -> Sym
mergeSym condition onTrue onFalse =
    SIte condition onTrue onFalse

instance Show Sym where
    show (SAdd x y)   = "(" <> show x <> " + " <> show y <> ")"
    show (SSub x y)   = "(" <> show x <> " - " <> show y <> ")"
    show (SDiv x y)   = "(" <> show x <> " / " <> show y <> ")"
    show (SMod x y)   = "(" <> show x <> " % " <> show y <> ")"
    show (SAbs x  )   = "|" <> show x <> "|"
    show (SConst x)   = show x
    show (SAnd x y)   = "(" <> show x <> " & " <> show y <> ")"
    show (SOr  x y)   = "(" <> show x <> " | " <> show y <> ")"
    show (SAny n  )   = "val_" <> show n
    show (SEq  x y)   = "(" <> show x <> " == " <> show y <> ")"
    show (SGt  x y)   = "(" <> show x <> " > " <> show y <> ")"
    show (SLt  x y)   = "(" <> show x <> " < " <> show y <> ")"
    show (SNot b )    = "¬" <> show b
    show (SIte i t e) = "ite(" <> show i <> ", " <> show t <> ", " <> show e <> ")"

-- | The state of symbolic computation
data SymState = SymState { registers         :: Map.Map Register Sym
                         , instructionCounter :: InstructionAddress -- Sym
                         , instructionRegister :: InstructionCode
                         , flags :: Map.Map Flag Sym
                         , memory :: Map.Map Word8 Sym
                         , program :: Program
                         , clock :: Clock

                         , pathConstraintList :: [Sym]
                         }

instance Show SymState where
    show state = unlines [ "IC: " <> show (instructionCounter state)
                         , "IR: " <> show (decode $ instructionRegister state)
                         , "Registers: " <> show (Map.toList $ registers state)
                         , "Flags: " <> show (Map.toList $ flags state)
                         , "Constraints: " <> show (pathConstraintList state)
                         ]

-- mergeStates :: SymState -> SymState -> SymState
-- mergeStates (SymState regs1 ic1 ir1 flags1 memory1 prog1 clock1 constraints1)
--             (SymState regs2 ic2 ir2 flags2 memory2 prog2 clock2 constraints2) =
--     let regs' = mergeSym <$> regs1 <*> regs2
--         ic' = ic
--         ir' =
--         flags1' =
--         memory1' =
--         prog1' =
--         clock1' =
--         constraints1' =


newtype Computation a = Computation { unComputation :: State SymState a }
-- newtype Computation a = Computation { unComputation :: State SymState [a] }

runComputation :: Computation a -> SymState -> (a, SymState)
runComputation (Computation c) initState = runState c initState

-- | The symbolic execution trace
type Trace = Tree.Tree SymState

emptyRegisters :: Map.Map Register Sym
emptyRegisters = Map.fromList $ zip [R0, R1, R2, R3] (map SConst [0, 0..])

emptyFlags :: Map.Map Flag Sym
emptyFlags = Map.fromList $ zip [Zero, Overflow, Halted] (map SConst [0, 0..])

initialiseMemory :: [(MemoryAddress, Sym)] -> Map.Map Word8 Sym
initialiseMemory vars =
    let blankMemory = Map.fromList $ zip [0..255] (map SConst [0, 0..])
    in foldr (\(addr, value) acc -> Map.adjust (const value) (fromIntegral addr) acc) blankMemory vars

boot :: Program -> Map.Map Word8 Sym -> SymState
boot prog mem = SymState { registers = emptyRegisters
                         , instructionCounter = 0
                         , instructionRegister = 0 -- encode $ Jump 0
                         , program = prog
                         , flags = emptyFlags
                         , memory = mem
                         , clock = 0

                         , pathConstraintList = []
                         }
