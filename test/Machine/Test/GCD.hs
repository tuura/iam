{-# LANGUAGE ScopedTypeVariables #-}

module Machine.Test.GCD where

import GHC.Generics
import System.IO.Unsafe (unsafePerformIO)
import Test.SmallCheck
import Test.SmallCheck.Series
import Text.Pretty.Simple
import qualified Data.Map.Strict as Map
import Machine.Types
import Machine.Instruction
import Machine.Instruction.Encode
import Machine.Instruction.Decode
import Machine.Program
import Machine.Semantics.SimulateITE

--------------------------------------------------------------------------------
prog :: Program
prog = unsafePerformIO $ readProgram "examples/gcd.asm"

gcdWorks :: IO ()
gcdWorks = do
    print "GCD works as 'Prelude.gcd' for positive arguments."
    let steps = 100
        machineGcd x y = let mem = initialiseMemory [(0, x), (1, y)]
                             initialState = boot prog mem
                             finalState = runModel steps initialState
                         in  snd . (\xs -> xs !! 1) . Map.toList $ registers finalState
    smallCheck 10 $ forAll $ \x ->
                    forAll $ \y ->
                        (x > 0 && y > 0) ==> (machineGcd x y == gcd x y)
