{-# LANGUAGE ConstraintKinds,
             RankNTypes #-}

module Machine.Examples.Common where

import qualified Data.Map as Map
import Machine.Types
import Machine.State
import Machine.Instruction
import Metalanguage
import Machine.Semantics
import Machine.Semantics.Dependencies
import Data.Maybe (fromJust)
import Algebra.Graph
import Data.Functor.Const
import Control.Applicative
import Data.List.NonEmpty hiding (zip)
import Text.Read (read)

emptyRegisters :: RegisterBank
emptyRegisters = Map.fromList [(R0, 0), (R1, 0), (R2, 0), (R3, 0)]

emptyFlags :: Flags
emptyFlags = Map.fromList [(Zero, 0), (Halted, 0)]

templateState :: Memory -> MachineState
templateState mem = MachineState { registers = emptyRegisters
                                 , instructionCounter = 0
                                 , instructionRegister = Jump 0
                                 , program = []
                                 , flags = emptyFlags
                                 , memory = mem
                                 , clock = 0
                                 }

ex1 :: Program -- [(InstructionAddress, Instruction)]
ex1 = zip [0..]
    [ Load R0 0
    , Add  R0 1
    -- , JumpZero 1
    -- , Add  R0 1
    ]



readProgram :: FilePath -> IO Program
readProgram = (fmap parseProgram) . readFile

parseProgram :: String -> Program
parseProgram = zip [0..] . Prelude.map read . lines

--------------------------------------------------------------------------------
-- ATAED'18 paper examples -----------------------------------------------------
--------------------------------------------------------------------------------

infixl 4 *+>

(*+>) :: Semantics Applicative k v a
      -> Semantics Applicative k v b
      -> Semantics Applicative k v b
(*+>) = (liftA2 (*>))


loadGraph = fromJust $ instructionGraph (0, (Load R0 0))
jumpGraph = fromJust $ instructionGraph (0, (Jump 42))
addGraph = fromJust $ instructionGraph (0, (Add R1 1))
wg = writeFile "ataed-2018-paper/img/loadJumpAdd.dot"
     (drawGraph $ overlays [loadGraph, jumpGraph, addGraph])


oracle1 = concurrencyOracle (semanticsA (Load R0 0)) (semanticsA (Load R1 2))

oracle2p1 = [Load R0 0, Add R0 1]
oracle2p2 = [Load R1 1]

oracle2 = concurrencyOracle (blockSemanticsA oracle2p1)
                            (blockSemanticsA oracle2p2)

oracle3p1 = [Load R0 0, Add R0 1, Jump 1]
oracle3p2 = [Load R1 1, Add R1 2, Jump 1]

oracle3 = concurrencyOracle (blockSemanticsA oracle3p1)
                            (blockSemanticsA oracle3p2)

tr  k    = Const [Left k]
tw k fv = Const [Right k] *> fv *> pure ()

-- ghci> load R0 0 tr tw *> load R1 1 tr tw
-- 
-- (fromJust $ load R0 0 tr tw) *> (fromJust $ load R1 1 tr tw)