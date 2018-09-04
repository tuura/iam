module Machine.Semantics.Symbolic.Examples where

import System.IO.Unsafe (unsafePerformIO)
import qualified Data.Tree as Tree
import Text.Pretty.Simple
import Machine.Instruction
import Machine.Instruction.Decode
import Machine.Instruction.Encode
import Machine.Program
import Machine.Types
import Machine.Semantics.Symbolic.Types
import Machine.Semantics.Symbolic hiding (readProgram)

assemble :: [Instruction] -> Program
assemble = zip [0..] . map (\i -> encode i)

addExample :: IO ()
addExample = do
    let prog = assemble $ [ Load R0 0
                          , Add  R0 1
                          , Halt
                          ]
        steps = 10
        x = SAny 0
        y = SAny 1
        mem = initialiseMemory [(0, x), (1, y)]
        initialState = boot prog mem
        trace = runModel steps initialState
    putStrLn $ Tree.drawTree $ fmap show $ trace

jzExample :: IO ()
jzExample = do
    let prog = assemble $ [ Load R0 0
                          , Add  R0 1
                          , JumpZero 1
                          , Halt
                          , Halt
                          ]
        steps = 100
        x = SAny 0
        y = SAny 1
        mem = initialiseMemory [(0, x), (1, y)]
        initialState = boot prog mem
        trace = runModel steps initialState
    putStrLn $ Tree.drawTree $ fmap show $ trace

gcdExample :: IO ()
gcdExample = do
    let prog = unsafePerformIO . readProgram $ "examples/gcd.asm"
        steps = 20
        -- x = SConst 2 -- SAny 0
        -- y = SConst 3 -- SAny 1
        x = SAny 0
        y = SAny 1
        mem = initialiseMemory [(0, x), (1, y)]
        initialState = boot prog mem
        trace = runModel steps initialState
    putStrLn $ Tree.drawTree $ fmap show $ trace