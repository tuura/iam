{-# LANGUAGE ScopedTypeVariables #-}

module Subtractor.Assembly where

import Data.SBV (mkSFunArray, writeArray)
import Control.Monad.Writer.Strict
import Subtractor.Types

type Script = Writer [Instruction] ()

-- newtype Label = Label Int

-- label :: Writer [Instruction] Label
-- label = pass (\(p :: [Instruction]) -> (Label (length p), p))

ld :: Register -> MemoryAddress -> Script
ld rX dmemaddr = tell [Ld rX dmemaddr]

ld_si :: Register -> SImm8 -> Script
ld_si rX simm = tell [Ld_si rX simm]

st :: Register -> MemoryAddress -> Script
st rX dmemaddr = tell [St rX dmemaddr]

sub :: Register -> MemoryAddress -> Script
sub rX dmemaddr = tell [Sub rX dmemaddr]

jmpi :: SImm10 -> Script
jmpi simm = tell [Jmpi simm]

jz :: SImm10 -> Script
jz simm = tell [Jz simm]

halt :: Script
halt = tell [Halt]

assemble :: Script -> Program
assemble s = zip [0..] prg
  where
    prg = snd $ runWriter s

-- assemble :: Script -> Program
-- assemble s = foldr (\(p, c) a -> writeArray a p c) a0 (zip [0..] $ map encode prg)
--   where
--     a0  = mkSFunArray (const 0)
--     prg = snd $ runWriter s
