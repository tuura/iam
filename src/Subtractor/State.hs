{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Subtractor.State where

import Data.SBV (Mergeable, symbolicMerge)
import Control.Monad.State.Strict
import GHC.Generics (Generic)
import Subtractor.Types
import Subtractor.Assembly

data MachineState = MachineState
    { registers           :: RegisterBank
    , instructionCounter  :: InstructionAddress
    , instructionRegister :: Instruction
    , flags               :: Flags
    , memory              :: Memory
    , program             :: Program
    , clock               :: Clock
    } deriving (Show)

instance Mergeable MachineState where
    symbolicMerge f t (MachineState rs1 ic1 ir1 fs1 m1 p1 c1)
                      (MachineState rs2 ic2 ir2 fs2 m2 p2 c2) =
                       MachineState rs  ic  ir  fs  m  p  c
      where
         rs = symbolicMerge f t rs1 rs2
         ic = symbolicMerge f t ic1 ic2
         ir = symbolicMerge f t ir1 ir2
         fs = symbolicMerge f t fs1 fs2
         m  = symbolicMerge f t m1 m2
         p  = symbolicMerge f t p1 p2
         c  = symbolicMerge f t c1 c2
