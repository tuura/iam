module Subtractor.Examples.Swap where

import Text.Pretty.Simple (pPrint)
import Control.Monad.Writer.Strict
import Data.SBV hiding (label)
import Subtractor
import Subtractor.Types
import Subtractor.State
import Subtractor.Assembly
import Subtractor.Examples.Common

swap :: Script
swap = do
    ld 0 0
    sub 0 1
    st 0 2
    halt

theoremSwap :: Symbolic SBool
theoremSwap = do
    x <- forall "x"
    y <- forall "y"
    let initialState = templateState swap (initialiseMemory [(0, x), (1, y)])
        finalState = verify steps $ initialState
    let z = readArray (memory finalState) 2
    pure $ z .== x - y

provingSwap :: IO ()
provingSwap = do
    result <- proveWith prover theoremSwap
    print result

simulatingSwap :: IO ()
simulatingSwap = do
    let finalState = verify steps $ templateState swap (initialiseMemory [(0, 42)])
        memoryDump = dumpMemory 0 5 $ memory finalState
    putStr "Memory Dump: "
    print memoryDump
    putStr "Flags register Dump: "
    print $ "Halted: " ++ show (readArray (flags finalState) (flagId Halted))
    putStr "Final state: "
    pPrint finalState

--------------------------------------------------------------------------------

countdown :: Script
countdown = do
    ld_si 0 1
    -- jmpi (-2)
    st 0 1
    ld 0 0
    sub 0 1
    st 0 0
    jz 1
    jmpi (-4)
    halt

-- countdown :: Script
-- countdown = do
--     ld_si 1 1
--     -- jmpi (-2)
--     st 1 2
--     ld 0 1
--     sub 0 2
--     st 0 1
--     jz 1
--     jmpi (-4)
--     halt

theoremCoundown :: Symbolic SBool
theoremCoundown = do
    x <- forall "x"
    constrain $ x .> 0
    constrain $ x .< 10
    -- y <- pure 0
    let initialState = templateState countdown (initialiseMemory [(0, x)])
        finalState = verify steps $ initialState
    let z = readArray (memory finalState) 0
    liftIO $ do putStrLn "Flags register Dump: "
                print $ "*  Halted: " ++ show (readArray (flags finalState) (flagId Halted))
                print $ "*  Zero: " ++ show (readArray (flags finalState) (flagId Zero))
                pPrint finalState
    pure $ z .== 0

provingCoundown :: IO ()
provingCoundown = do
    result <- proveWith prover theoremCoundown
    print result

simulatingCountdown :: IO ()
simulatingCountdown = do
    putStrLn "Program: "
    pPrint $ zip [0..] $ snd $ runWriter countdown
    let finalState = verify steps $ templateState countdown (initialiseMemory [(0, 8)])
        memoryDump = dumpMemory 0 5 $ memory finalState
    putStrLn "Final state: "

    putStrLn $ "Instruction register: " ++ (show $ instructionRegister finalState)
    putStrLn "Flags register Dump: "
    print $ "*  Halted: " ++ show (readArray (flags finalState) (flagId Halted))
    print $ "*  Zero: " ++ show (readArray (flags finalState) (flagId Zero))
    -- print $ "*  OutOfProgram: " ++ show (readArray (flags finalState) (flagId OutOfProgram))

    putStr "Memory Dump: "
    print memoryDump
    pPrint finalState