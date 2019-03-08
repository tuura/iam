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

data Sym a where
    SAdd   :: (Val -> a) -> Sym Val -> Sym Val -> Sym Val
    SSub   :: (Val -> a) -> Sym Val -> Sym Val -> Sym Val
    SDiv   :: (Val -> a) -> Sym Val -> Sym Val -> Sym Val
    SMod   :: (Val -> a) -> Sym Val -> Sym Val -> Sym Val
    SConst :: a -> Sym a
    SAnd   :: (Bool -> a) -> Sym Bool -> Sym Bool -> Sym Bool
    SOr    :: (Bool -> a) -> Sym Bool -> Sym Bool -> Sym Bool
    SAny   :: Int -> Sym Val     -- Any value or the set of all values
    SEq    :: (Bool -> a) -> Sym Val -> Sym Val -> Sym Bool
    SGt    :: (Bool -> a) -> Sym Val -> Sym Val -> Sym Bool
    SLt    :: (Bool -> a) -> Sym Val -> Sym Val -> Sym Bool
    SNot   :: (Bool -> a) -> Sym Bool -> Sym Bool

instance Functor Sym where
  fmap f (SAdd t x y) = SAdd (f . t) (f . t <$> x) (f . t <$> y)
--   fmap f (SSub t x y) = SSub (f . t) x y
--   fmap f (SDiv t x y) = SDiv (f . t) x y
--   fmap f (SMod t x y) = SMod (f . t) x y
--   fmap SConst
--   fmap SAnd
--   fmap SOr
--   fmap SAny
--   fmap SEq
--   fmap SGt
--   fmap SLt
--   fmap SNot

sNot :: Sym Bool -> Sym Bool
sNot = SNot id

sConst :: a -> Sym a
sConst = SConst

sEq :: Sym Val -> Sym Val -> Sym Bool
sEq x y = SEq id x y

sGt :: Sym Val -> Sym Val -> Sym Bool
sGt x y = SGt id x y

sLt :: Sym Val -> Sym Val -> Sym Bool
sLt x y = SLt id x y

-- instance Show a => Show (SymImpl a) where
--     show (SAdd x y)   = "(" <> show x <> " + " <> show y <> ")"
--     show (SSub x y)   = "(" <> show x <> " - " <> show y <> ")"
--     show (SDiv x y)   = "(" <> show x <> " / " <> show y <> ")"
--     show (SMod x y)   = "(" <> show x <> " % " <> show y <> ")"
--     show (SAbs x  )   = "|" <> show x <> "|"
--     show (SConst x)   = show x
--     show (SAnd x y)   = "(" <> show x <> " & " <> show y <> ")"
--     show (SOr  x y)   = "(" <> show x <> " | " <> show y <> ")"
--     show (SAny n  )   = "val_" <> show n
--     show (SEq  x y)   = "(" <> show x <> " == " <> show y <> ")"
--     show (SGt  x y)   = "(" <> show x <> " > " <> show y <> ")"
--     show (SLt  x y)   = "(" <> show x <> " < " <> show y <> ")"
--     show (SNot b )    = "¬" <> show b

instance Show a => Show (Sym a) where
    show (SAdd t x y) = "(" <> show x <> " + " <> show y <> ")"
    show (SSub t x y) = "(" <> show x <> " - " <> show y <> ")"
    show (SDiv t x y) = "(" <> show x <> " / " <> show y <> ")"
    show (SMod t x y) = "(" <> show x <> " % " <> show y <> ")"
        -- (SAbs x  )   -> "|" <> show x <> "|"
    show (SConst x) = show x
    show (SAnd t x y) = "(" <> show x <> " & " <> show y <> ")"
    show (SOr  t x y) = "(" <> show x <> " | " <> show y <> ")"
    show (SAny n  ) = "val_" <> show n
    show (SEq  t x y) = "(" <> show x <> " == " <> show y <> ")"
    show (SGt  t x y) = "(" <> show x <> " > " <> show y <> ")"
    show (SLt  t x y) = "(" <> show x <> " < " <> show y <> ")"
    show (SNot t b )  = "¬" <> show b

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
    zip [R0, R1, R2, R3] (map SConst [0, 0..])

emptyFlags :: Map.Map Flag (Sym Bool)
emptyFlags = Map.fromList $ zip [Zero, Overflow, Halted] (map SConst $ repeat False)

initialiseMemory :: [(MemoryAddress, (Sym Val))] -> Map.Map Word8 (Sym Val)
initialiseMemory vars =
    let blankMemory = Map.fromList $ zip [0..255] (map SConst [0, 0..])
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
