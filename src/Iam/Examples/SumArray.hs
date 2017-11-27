module Subtractor.Examples.SumArray where

import Prelude hiding (subtract)
import Text.Pretty.Simple (pPrint)
import Data.SBV hiding (label)
import Subtractor
import Subtractor.Types
import Subtractor.State
import Subtractor.Assembly
import Subtractor.Semantics (readRegister)
import Subtractor.Examples.Common

--------------------------------------------------------------------------------
sumArray :: Script
sumArray = do
    load r0 0       -- sum := 0, sum accumulator
    load r2 254     -- i := n,   loop counter
    loop <- label
    store r2 255
    loadMI r1 255   -- load a[i]
    store r1 254    -- put a[i] to cell 254
    add r0 254      -- sum := sum + a[i]
    add r2 253      -- i := i - 1
    jumpZero 1
    goto loop
    halt

theoremSumArray :: Int -> IO ThmResult
theoremSumArray n = proveWith prover $ do
    summands <- mkForallVars n
    let memory = initialiseMemory $  [(0, 0)]
                                  ++ zip [1..] summands
                                  ++ [ (253, -1)
                                     , (254, fromIntegral n)
                                     ]
        steps = 10000
        finalState = verify steps $ templateState sumArray memory
        result = readArray (registers finalState) 0
    pure $ result .== sum summands

simulateSumArray :: IO ()
simulateSumArray = do
    putStrLn "Program: "
    -- pPrint $ zip [0..] $ snd $ runWriter (arraySum 5)
    let mem = initialiseMemory [(1, 64), (2, 64), (3, 32), (253, -1), (254, 3)]
        finalState = verify 1000 $ templateState sumArray mem
        memoryDump = dumpMemory 0 5 $ memory finalState
    putStrLn "Final state: "

    putStrLn $ "Instruction register: " ++ (show $ instructionRegister finalState)
    putStrLn "Flags register Dump: "
    print $ "*  Halted: " ++ show (readArray (flags finalState) (flagId Halted))
    print $ "*  Zero: " ++ show (readArray (flags finalState) (flagId Zero))
    -- print $ "*  OutOfProgram: " ++ show (readArray (flags finalState) (flagId OutOfProgram))

    putStr "Memory Dump: "
    print memoryDump
    putStrLn $ "R0: " ++ show (readArray (registers finalState) 0)
    putStrLn $ "R1 = " ++ show (readArray (registers finalState) 1)
    putStrLn $ "R2 = " ++ show (readArray (registers finalState) 2)
    pPrint finalState