module Subtractor.Examples.SubtractArray where

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

subtractArray :: Script
subtractArray = do
    load r0 0       -- sum := 0, sum accumulator
    load r2 254     -- i := n,   loop counter
    loop <- label
    store r2 255
    loadMI r1 255   -- load a[i]
    store r1 254    -- put a[i] to cell 254
    subtract r0 254 -- sum := sum - a[i]
    subtract r2 253 -- i := i - 1
    jumpZero 1
    goto loop
    halt

theoremSubtractArray :: Int -> IO ThmResult
theoremSubtractArray n = proveWith prover $ do
    minuend <- forall "minuend"
    subtrahends <- mkForallVars n
    let memory = initialiseMemory $  [(0, minuend)]
                                  ++ zip [1..] subtrahends
                                  ++ [ (253, 1)
                                     , (254, fromIntegral n)
                                     ]
        steps = 10000
        finalState = verify steps $ templateState subtractArray memory
        result = readArray (registers finalState) 0
    pure $ result .== foldl (\acc x -> acc - x) minuend subtrahends

simulatingSubtractArray :: IO ()
simulatingSubtractArray = do
    putStrLn "Program: "
    -- pPrint $ zip [0..] $ snd $ runWriter (arraySum 5)
    let mem = initialiseMemory [(1, 64), (2, 64), (3, 32), (253, 1), (255, 302)]
        finalState = verify 1000 $ templateState subtractArray mem
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