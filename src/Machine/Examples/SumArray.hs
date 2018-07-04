{-# LANGUAGE ConstraintKinds, RankNTypes,
             ScopedTypeVariables,
             FlexibleContexts,
             FlexibleInstances,
             TypeFamilies #-}
module Machine.Examples.SumArray where

import System.IO.Unsafe (unsafePerformIO)
import Control.Monad.IO.Class (liftIO)
import Data.Maybe (fromJust)
import Prelude hiding (Monad, subtract)
import qualified Prelude (Monad)
import Text.Pretty.Simple (pPrint)
import Data.SBV hiding (label)
import Control.Selective
import Metalanguage
import Machine.SemanticsNum (semanticsM, MachineKey, executeInstruction)
import Machine.Semantics.Symbolic
import Machine.Semantics.Symbolic.Machine (run)
import Machine.Semantics.Symbolic.Types
import Machine.Semantics.Symbolic.Instruction
import Machine.Semantics.Symbolic.State
import Machine.Assembly
import Machine.Value
-- import Machine.Semantics (readRegister)
import Machine.Examples.Common

-- prover = z3 { verbose = True
--             , redirectVerbose = Just "example.smt2"
--             , timing = PrintTiming
--             , printBase = 10
--             }

-- initialiseMemory :: [(SBV MemoryAddress, SBV Value)] -> Memory
-- initialiseMemory =
--     foldr (\(a, v) m -> writeArray m a v) (mkSFunArray $ const 0)

type Monad m = (Selective m, Prelude.Monad m)

-- runModel :: Int -> MachineState -> MachineState
-- runModel steps state
--     | steps == 0 = state
--     | otherwise  = ite halted state (runModel (steps - 1) nextState)
--   where
--     halted    = (./= 0) $ readArray (flags state) (literal Halted)
--     nextState = snd $ run executeInstruction state

state1 = templateState (assemble addInstruction) mem
    where mem = initialiseMemory [(0, 2), (1, 3)]

runModel :: Int -> MachineState -> MachineState
runModel steps state
    | steps == 0 = state
    | otherwise  = if halted then state else (runModel (steps - 1) nextState)
  where
    halted    = (/= 0) $ readArray (flags state) (literal Halted)
    nextState =
        let ast = fromJust $ buildAST executeInstruction
        in snd $ run (interpretSymbolic ast) state

-- simulateScriptViaMetalanguage :: MachineState -> MachineState
-- simulateScriptViaMetalanguage src state =
--     let ast = fromJust $ buildAST (blockSemanticsM . instructions $ src)
--     in snd $ run (interpretSymbolic ast) state

-- simulateScriptViaMetalanguage :: Script -> MachineState -> MachineState
-- simulateScriptViaMetalanguage src state =
--     let ast = fromJust $ buildAST (blockSemanticsM . instructions $ src)
--     in snd $ run (interpretSymbolic ast) state

-- simulateScriptViaMetalanguage :: (addr ~ byte)
--     => [Instruction (SBV Register) (SBV MemoryAddress) (SBV Flag) (SBV Byte)] -> MachineState -> MachineState
-- simulateScriptViaMetalanguage src state =
--     let ast = fromJust $ buildAST (bsM src)
--     in snd $ run (interpretSymbolic ast) state

-- bsM :: (Num v, Eq v, IsRegister r, IsMemoryAddress addr, IsFlag flag, v ~ addr)
--     => [Instruction r addr flag v]
--     -> Semantics Monad (MachineKey r addr iaddr flag) v ()
-- bsM xs = \read write ->
--     foldr (\x acc -> ((>>)) <$> acc <*> semanticsM x read write) nop xs
--     where nop = Just $ pure ()

--------------------------------------------------------------------------------
sumArray :: Script
sumArray = do
    load R0 0       -- sum := 0, sum accumulator
    load R2 254     -- i := n,   loop counter
    loop <- label
    store R2 255
    loadMI R1 255   -- load a[i]
    store R1 254    -- put a[i] to cell 254
    add R0 254      -- sum := sum + a[i]
    add R2 253      -- i := i - 1
    jumpZero 1
    goto loop
    halt

sumArrayInstructions :: [Instruction (SBV Register) (SBV MemoryAddress) (SBV Flag) (SBV Byte)]
sumArrayInstructions = [ Load (literal R0) 0
                       , Load (literal R2) 254
                       , Store (literal R2) 255
                       , LoadMI (literal R1) 255
                       , Store (literal R1) 254
                       , Add (literal R0) 254
                       , Add (literal R2) 253
                       , JumpZero 1
                       , Jump (-7)
                       , Halt
                       ]

-- addInstruction :: [Instruction (SBV Register) (SBV MemoryAddress) (SBV Flag) (SBV Byte)]
addInstruction :: [Instruction Register MemoryAddress Flag Byte]
addInstruction = reverse $
                 [ -- LoadMI R0 0
                 Load R0 0
                 , Add R0 1
                 , Jump 1
                 ]

-- theoremAdd  :: Symbolic SBool
-- theoremAdd = do
--     let x = literal 1
--         y = literal 4
--     -- x <- forall "x"
--     -- y <- forall "y"
--     let mem = initialiseMemory [(0, x), (1, y)]
--         -- steps = 1000
--         finalState =
--             simulateScriptViaMetalanguage addInstruction
--                 (templateState (mkSFunArray $ const (literal $ Jump 0)) mem)
--         result = readArray (registers finalState) (literal R0)
--     liftIO $ print result
--     -- liftIO $ print $ readArray (memory finalState) (literal 1)
--     pure $ result .== y
-- theoremSumArray :: Int -> IO ThmResult
-- theoremSumArray n = proveWith prover $ do
--     summands <- mkForallVars n
--     let memory = initialiseMemory $  [(0, 0)]
--                                   ++ zip [1..] summands
--                                   ++ [ (253, -1)
--                                      , (254, fromIntegral n)
--                                      ]
--         steps = 10000
--         finalState = runModel steps $ templateState (assemble sumArray) memory
--         result = readArray (registers finalState) (literal R0)
--     pure $ result .== sum summands &&& clock finalState .< 10000

-- metalanguageSumArrayTheorem :: Int64 -> Symbolic SBool
-- metalanguageSumArrayTheorem n = do
--     -- summands <- mkForallVars n
--     let summands = map literal [1..n]
--     let memory = initialiseMemory $  [(0, 0)]
--                                   ++ zip [1..] summands
--                                   ++ [ (253, -1)
--                                      , (254, fromIntegral n)
--                                      ]
--         steps = 10000
--         finalState =
--             simulateScriptViaMetalanguage sumArrayInstructions (templateState (mkSFunArray $ const (literal Halt)) memory)
--         result = readArray (registers finalState) (literal R0)
--     liftIO $ print finalState
--     pure $ result .== sum summands -- &&& clock finalState .< 10000