{-# LANGUAGE DeriveFunctor #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Iam.Assembly
-- Copyright   :  (c) Georgy Lukyanov, Andrey Mokhov 2017
--
-- Maintainer  :  lukyanov.georgy@gmail.com
-- Stability   :  experimental
--
-- An embedded assembly language of Inglorious Adding Machine.
--
--------------------------------------------------------------------------------
module Iam.Assembly where

import Control.Monad (ap)
import Iam.Types

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

write :: Instruction -> Script
write i = Writer (\p -> ((), i:p))

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
load rX dmemaddr = write (Load rX dmemaddr)

-- | Load a value from a memory location to a register,
--   using the memory indirect access mode
loadMI :: Register -> MemoryAddress -> Script
loadMI rX dmemaddr = write (LoadMI rX dmemaddr)

-- | Load an immediate value to a register
set :: Register -> SImm8 -> Script
set rX simm = write (Set rX simm)

-- | Store a value from a register to a memory location
store :: Register -> MemoryAddress -> Script
store rX dmemaddr = write (Store rX dmemaddr)

-- | Add a value loaded from a memory location to one stored in
--   a register
add :: Register -> MemoryAddress -> Script
add rX dmemaddr = write (Add rX dmemaddr)

-- | Perform an unconditional jump
jump :: SImm10 -> Script
jump simm = write (Jump simm)

-- | Perform a jump if 'Zero' flag is set
jumpZero :: SImm10 -> Script
jumpZero simm = write (JumpZero simm)

-- | Stop the machine operation
halt :: Script
halt = write (Halt)

-- | Translate the script source code to a program --- a list of instructions
---  with assigned instruction addresses
assemble :: Script -> Program
assemble s = zip [0..] prg
  where
    prg = reverse $ snd $ runWriter s []
