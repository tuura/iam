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
import Machine.Instruction
import Machine.Semantics (semanticsM, MachineKey, executeInstruction)
import Machine.Semantics.Symbolic
import Machine.Semantics.Symbolic.Machine (run)
import Machine.Semantics.Symbolic.Types
import Machine.Semantics.Symbolic.State
import Machine.Assembly
import Machine.Value
import Machine.Examples.Common

type Monad m = (Selective m, Prelude.Monad m)
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

sumArrayInstructions :: [Instruction Register MemoryAddress Flag Byte]
sumArrayInstructions = [ Load (R0) 0
                       , Load (R2) 254
                       , Store (R2) 255
                       , LoadMI (R1) 255
                       , Store (R1) 254
                       , Add (R0) 254
                       , Add (R2) 253
                       , JumpZero 10
                       , Jump 2
                       , Halt
                       ]

theoremSumArray :: Int -> Int -> Symbolic SBool
theoremSumArray steps n = do
    summands <- mkForallVars n
    -- let summands = [1..fromIntegral n] -- replicate n (literal (1 :: Value))
    let mem = initialiseMemory $  [(0, 0)]
                                  ++ zip [1..] summands
                                  ++ [ (253, -1)
                                     , (254, fromIntegral n)
                                     ]
        -- steps = 1
        finalState = runModel steps $ templateState (assemble sumArrayInstructions) mem
        result = readArray (registers finalState) (literal R0)
    -- liftIO $ print result
    -- liftIO $ putStrLn $ "Summands: " ++ show summands
    -- liftIO $ putStrLn $ "R0: " ++ show (readArray (registers finalState) (literal R0))
    -- liftIO $ putStrLn $ "R1: " ++ show (readArray (registers finalState) (literal R1))
    -- liftIO $ putStrLn $ "R2: " ++ show (readArray (registers finalState) (literal R2))
    -- liftIO $ print (readArray (registers finalState) (literal R0))
    -- liftIO $ putStrLn $ "Zero: " ++ show (readArray (flags finalState) (literal Zero))
    -- liftIO $ putStrLn $ "Mem 3: " ++ show (readArray (memory finalState) (literal 3))
    -- liftIO $ pPrint finalState
    pure $ result .== sum summands -- &&& clock finalState .< 10000
