module Machine.Examples.Max where

import Prelude hiding (subtract)
import Text.Pretty.Simple (pPrint)
import Data.SBV hiding (label)
import Machine
import Machine.Types
import Machine.State
import Machine.Assembly
import Machine.Semantics (readRegister)
import Machine.Examples.Common

max' :: Script
max' = do
    load r0 0       -- sum := 0, sum accumulator
    cmpGT r0 1
    jumpGT 1
    halt
    load R1 42
    store R1 2
    halt

theoremMax :: Value -> Value -> IO ThmResult
theoremMax a b = proveWith prover $ do
    a <- forall "a"
    b <- forall "b"
    let mem = initialiseMemory $  [(0, a), (1, b), (2, 0)]
        steps = 10000
        finalState = runModel steps $ templateState max' mem
        result = readArray (memory finalState) (literal 2)
    pure $ (result .== literal (42 :: Value)) .== (a .> b)