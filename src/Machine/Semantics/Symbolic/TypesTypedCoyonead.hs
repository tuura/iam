{-# LANGUAGE GADTs, DeriveFunctor, StandaloneDeriving, TypeApplications #-}

module Machine.Semantics.Symbolic.TypesTyped where

import Metalanguage
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
import Data.Int (Int16)

type Val = Int16

data SymImpl a where
    SAdd   :: SymImpl Val -> SymImpl Val -> SymImpl Val
    SSub   :: SymImpl Val -> SymImpl Val -> SymImpl Val
    SDiv   :: SymImpl Val -> SymImpl Val -> SymImpl Val
    SMod   :: SymImpl Val -> SymImpl Val -> SymImpl Val
    SAbs   :: SymImpl Val -> SymImpl Val
    SConst :: a -> SymImpl a
    SAnd   :: SymImpl Bool -> SymImpl Bool -> SymImpl Bool
    SOr    :: SymImpl Bool -> SymImpl Bool -> SymImpl Bool
    SAny   :: Int -> SymImpl Val     -- Any value or the set of all values
    SEq    :: SymImpl Val -> SymImpl Val -> SymImpl Bool
    SGt    :: SymImpl Val -> SymImpl Val -> SymImpl Bool
    SLt    :: SymImpl Val -> SymImpl Val -> SymImpl Bool
    SNot   :: SymImpl Bool -> SymImpl Bool

data Sym a where
    MkSym :: (x -> a) -> SymImpl x -> Sym a

deriving instance Functor Sym

sConst :: a -> Sym a
sConst x = MkSym id (SConst x)

l :: SymImpl a -> Sym a
l = MkSym id

sNot :: Sym Bool -> Sym Bool
sNot (MkSym t x) = MkSym t (SNot x)

-- data Sym a where
--     SAdd   :: Sym Val -> Sym Val -> Sym (Val -> a)
--     SSub   :: Sym Val -> Sym Val -> Sym (Val -> a)
--     SDiv   :: Sym Val -> Sym Val -> Sym (Val -> a)
--     SMod   :: Sym Val -> Sym Val -> Sym (Val -> a)
--     SAbs   :: Sym Val -> Sym (Val -> a)
--     SConst :: a -> Sym a
--     SAnd   :: Sym Bool -> Sym Bool -> Sym (Bool -> a)
--     SOr    :: Sym Bool -> Sym Bool -> Sym (Bool -> a)
--     SAny   :: Int -> Sym (Val -> a)     -- Any value or the set of all values
--     SEq    :: Sym Val -> Sym Val -> Sym (Bool -> a)
--     SGt    :: Sym Val -> Sym Val -> Sym (Bool -> a)
--     SLt    :: Sym Val -> Sym Val -> Sym (Bool -> a)
--     SNot   :: Sym Bool -> Sym (Bool -> a)

instance Show a => Show (SymImpl a) where
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

instance Show a => Show (Sym a) where
    show (MkSym t z) = case z of
        (SAdd x y)   -> "(" <> show x <> " + " <> show y <> ")"
        (SSub x y)   -> "(" <> show x <> " - " <> show y <> ")"
        (SDiv x y)   -> "(" <> show x <> " / " <> show y <> ")"
        (SMod x y)   -> "(" <> show x <> " % " <> show y <> ")"
        (SAbs x  )   -> "|" <> show x <> "|"
        (SConst x)   -> show (t x)
        (SAnd x y)   -> "(" <> show x <> " & " <> show y <> ")"
        (SOr  x y)   -> "(" <> show x <> " | " <> show y <> ")"
        (SAny n  )   -> "val_" <> show n
        (SEq  x y)   -> "(" <> show x <> " == " <> show y <> ")"
        (SGt  x y)   -> "(" <> show x <> " > " <> show y <> ")"
        (SLt  x y)   -> "(" <> show x <> " < " <> show y <> ")"
        (SNot b )    -> "¬" <> show b

-- | The state of symbolic computation
data SymState = SymState { registers         :: Map.Map Register (Sym Val)
                         , instructionCounter :: InstructionAddress -- Sym
                         , instructionRegister :: InstructionCode
                         , flags :: Map.Map Flag (Sym Bool)
                         , memory :: Map.Map Word8 (Sym Val)
                         , program :: Program
                         , clock :: Clock

                         , pathConstraintList :: [Sym Bool]
                         }

instance Show SymState where
    show state = unlines [ "IC: " <> show (instructionCounter state)
                         , "IR: " <> show (decode $ instructionRegister state)
                         , "Registers: " <> show (Map.toList $ registers state)
                         , "Flags: " <> show (Map.toList $ flags state)
                         , "Constraints: " <> show (pathConstraintList state)
                         ]

-- | The symbolic execution trace
type Trace = Tree.Tree SymState

emptyRegisters :: Map.Map Register (Sym Val)
emptyRegisters = Map.fromList $
    zip [R0, R1, R2, R3] (map sConst [0, 0..])

emptyFlags :: Map.Map Flag (Sym Bool)
emptyFlags = Map.fromList $ zip [Zero, Overflow, Halted] (map sConst $ repeat False)

initialiseMemory :: [(MemoryAddress, (Sym Val))] -> Map.Map Word8 (Sym Val)
initialiseMemory vars =
    let blankMemory = Map.fromList $ zip [0..255] (map sConst [0, 0..])
    in foldr (\(addr, value) acc -> Map.adjust (const value) (fromIntegral addr) acc) blankMemory vars

boot :: Program -> Map.Map Word8 (Sym Val) -> SymState
boot prog mem = SymState { registers = emptyRegisters
                         , instructionCounter = 0
                         , instructionRegister = 0 -- encode $ Jump 0
                         , program = prog
                         , flags = emptyFlags
                         , memory = mem
                         , clock = 0

                         , pathConstraintList = []
                         }
