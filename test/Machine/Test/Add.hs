{-# LANGUAGE ScopedTypeVariables #-}

module Machine.Test.Add where

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
import Machine.Semantics.Simulate

-- deriving instance Generic

--------------------------------------------------------------------------------
prog :: Program
prog = unsafePerformIO $ readProgram "examples/add.asm"

addWorks :: IO ()
addWorks = do
    print "Add works as '+'."
    let steps = 3
        machineAdd x y = let mem = initialiseMemory [(0, x), (1, y)]
                             initialState = boot prog mem
                             finalState = runModel steps initialState
                         in  snd . head . Map.toList $ registers finalState
    smallCheck 10 $ forAll $ \x ->
                    forAll $ \y -> machineAdd x y == x + y

addOverflow :: IO ()
addOverflow = do
    print "Add triggers overflow flag for 'maxBound + 1'."
    let mem = initialiseMemory [(0, maxBound), (1, 1)]
        initialState = boot prog mem
        finalState = runModel 3 initialState
    print . registers $ finalState
    print . flags $ finalState

-- addExample :: Int -> Value -> Value -> IO ()
-- addExample steps x y = do
--     prog <- readProgram "examples/add.asm"
--     let mem = initialiseMemory [(0, x), (1, y)]
--         initialState = boot prog mem
--         finalState = runModel steps initialState
--     print . registers $ finalState
--     print . decode . instructionRegister $ finalState
--     print . instructionCounter $ finalState
--     print . flags $ finalState
--     -- pPrint $ finalState