module Machine.Examples.SumArray where

import Prelude hiding (subtract)
import Text.Pretty.Simple (pPrint)
import Data.SBV hiding (label)
import Machine
import Machine.Types
import Machine.State
import Machine.Assembly
import Machine.Semantics (readRegister)
import Machine.Examples.Common

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
        finalState = runModel steps $ templateState sumArray memory
        result = readArray (registers finalState) (literal R0)
    pure $ result .== sum summands &&& clock finalState .< 10000
