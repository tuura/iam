{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}


module Subtractor.Assembly where

import Control.Monad.State.Strict (modify)
import Data.SBV
import Control.Monad.Free
import Control.Monad.Free.TH (makeFree)
import Subtractor.Types
import Subtractor.Verification

data Assembly next = Halt
                   | Ld    next Register MemoryAddress
                   | Ld_si next Register SImm8
                   | St    next Register MemoryAddress
                   | Sub   next Register MemoryAddress
                   | Jmpi  next SImm10
    deriving (Functor)

makeFree ''Assembly

type Script = Free Assembly ()

showScript :: Script -> [String]
showScript (Free Halt) =
    ["halt"]
showScript (Free (Ld next rX dmemaddr)) =
    ("ld " ++ show rX ++ " " ++ show dmemaddr):showScript next
showScript (Free (Ld_si next rX simm)) =
    ("ld_si " ++ show rX ++ " " ++ show simm):showScript next
showScript (Free (St next rX dmemaddr)) =
    ("st " ++ show rX ++ " " ++ show dmemaddr):showScript next
showScript (Free (Sub next rX dmemaddr)) =
    ("sub " ++ show rX ++ " " ++ show dmemaddr):showScript next

--------------------------------------------------------------------------------

execScript :: Script -> Machine ()
execScript (Free command) = do
    incrementInstructionCounter
    case command of
        Halt                 ->
            writeFlag Halted true
        Ld next rX dmemaddr  ->
            readMemory dmemaddr >>= writeRegister rX >> execScript next
        Ld_si next rX simm   ->
            (writeRegister rX $ fromSImm8 simm) >> execScript next
        St next rX dmemaddr  ->
            readRegister rX >>= writeMemory dmemaddr >> execScript next
        Sub next rX dmemaddr -> do
            writeRegister rX <~ (readRegister rX, (-), readMemory dmemaddr)
            execScript next
        Jmpi next simm       -> do
            modify $ \currentState ->
                currentState {instructionCounter =
                    instructionCounter currentState + fromSImm10 simm}
            execScript $ skip (fromSImm10 simm) next

-- skip :: Int -> Script -> Script
skip 0 src = src
skip n src@(Free command) =
    case command of
         Halt       -> src
         Ld    next _ _ -> skip (n - 1) next
         Ld_si next _ _ -> skip (n - 1) next
         St    next _ _ -> skip (n - 1) next
         Sub   next _ _ -> skip (n - 1) next
         Jmpi  next _   -> skip (n - 1) next

(<~) :: (c -> Machine ()) -> (Machine a, a -> b -> c, Machine b) -> Machine ()
(<~) res (arg1, op, arg2) = do
    x <- arg1
    y <- arg2
    res $ x `op` y

fromSImm8 :: SImm8 -> Value
fromSImm8 s = fromBitsLE $ blastLE s ++ replicate 56 (sTestBit s 7)

fromSImm10 :: SImm10 -> InstructionAddress
fromSImm10 s = fromBitsLE $ (take 10 $ blastLE s) ++ replicate 6 (sTestBit s 9)

--------------------------------------------------------------------------------
