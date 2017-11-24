{-# LANGUAGE DeriveFunctor #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Subtractor.Assembly
-- Copyright   :  (c) Georgy Lukyanov, Andrey Mokhov 2017
--
-- Maintainer  :  lukyanov.georgy@gmail.com
-- Stability   :  experimental
--
-- An embedded SUBTRACTOR assembly language.
--
--------------------------------------------------------------------------------
module Subtractor.Assembly where

import Control.Monad (ap)
import Subtractor.Types

-- | An assembly writer monad.
newtype Writer a = Writer {runWriter :: [Instruction] -> (a, [Instruction])}
    deriving Functor

type Script = Writer ()

instance Applicative Writer where
    pure  = return
    (<*>) = ap

instance Monad Writer where
    return a       = Writer (\p -> (a, p))
    Writer w >>= f = Writer (\p -> let (a, p') = w p in runWriter (f a) p')

tell :: [Instruction] -> Script
tell i = Writer (\p -> ((), i ++ p))

newtype Label = Label Int

label :: Writer Label
label = Writer (\p -> (Label (length p), p))

goto :: Label -> Script
goto (Label there) = do
    Label here <- label
    let offset = fromIntegral (there - here - 1)
    jump offset -- TODO: Add error handling if offset is too large

-- | Load a value from a memory location to a register
load :: Register -> MemoryAddress -> Script
load rX dmemaddr = tell [Load rX dmemaddr]

-- | Load a value from a memory location to a register,
--   using the memory indirect access mode
loadMI :: Register -> MemoryAddress -> Script
loadMI rX dmemaddr = tell [LoadMI rX dmemaddr]

-- | Load an immediate value to a register
set :: Register -> SImm8 -> Script
set rX simm = tell [Set rX simm]

-- | Store a value from a register to a memory location
store :: Register -> MemoryAddress -> Script
store rX dmemaddr = tell [Store rX dmemaddr]

-- | Subtract a value loaded from a memory location from one stored in
--   a register
subtract :: Register -> MemoryAddress -> Script
subtract rX dmemaddr = tell [Subtract rX dmemaddr]

-- | Perform an unconditional jump
jump :: SImm10 -> Script
jump simm = tell [Jump simm]

-- | Perform a jump if 'Zero' flag is set
jumpZero :: SImm10 -> Script
jumpZero simm = tell [JumpZero simm]

-- | Stop the machine operation
halt :: Script
halt = tell [Halt]

-- | Translate the script source code to a program --- a list of instructions
---  with assigned instruction addresses
assemble :: Script -> Program
assemble s = zip [0..] prg
  where
    prg = reverse $ snd $ runWriter s []
