{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Subtractor.State
-- Copyright   :  (c) Georgy Lukyanov, Andrey Mokhov 2017
--
-- Maintainer  :  lukyanov.georgy@gmail.com
-- Stability   :  experimental
--
-- The state of a SUBTRACTOR machine.
--
--------------------------------------------------------------------------------
module Subtractor.State where

import Data.SBV (Mergeable, symbolicMerge)
import Control.Monad.State.Strict
import GHC.Generics (Generic)
import Subtractor.Types
import Subtractor.Assembly

-- | The state of a SUBTRACTOR machine
data MachineState = MachineState
    { registers           :: RegisterBank
    , instructionCounter  :: InstructionAddress
    , instructionRegister :: Instruction
    , flags               :: Flags
    , memory              :: Memory
    , program             :: Program
    , clock               :: Clock
    } deriving (Show, Generic, Mergeable)
