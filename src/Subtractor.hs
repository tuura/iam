module Subtractor where

import Text.Pretty.Simple (pPrint)
import Data.SBV
import Subtractor.Types
import Subtractor.Assembly

emptyRegisters :: RegisterBank
emptyRegisters = mkSFunArray $ const 0

emptyFlags :: Flags
emptyFlags = mkSFunArray $ const false

initialiseMemory :: [(MemoryAddress, Value)] -> Memory
initialiseMemory =
    foldr (\(a, v) m -> writeArray m a v) (mkSFunArray $ const 0)

dumpMemory :: Word8 -> Word8 -> Memory -> [SWord64]
dumpMemory from to m = map (readArray m) [literal from..literal to]

templateState :: Memory -> MachineState
templateState m = MachineState { registers = emptyRegisters
                               , instructionCounter = 0
                               , flags = emptyFlags
                               , memory = m
                               , clock = 0
                               }

exampleScript :: Script
exampleScript = do
    ld_si 1 42
    jmpi 1
    st 1 0
    st 1 1
    halt

f :: IO ()
f = do
    let finalState = snd $ execute (execScript exampleScript)
                                   (templateState (initialiseMemory []))
        memoryDump = dumpMemory 0 5 $ memory finalState
    putStr "Memory Dump: "
    print memoryDump
    putStr "Flags register Dump: "
    print $ "Halted: " ++ show (readArray (flags finalState) (flagId Halted))
    putStr "Final state: "
    pPrint finalState