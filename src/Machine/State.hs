{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Machine.State
-- Copyright   :  (c) Georgy Lukyanov, Andrey Mokhov 2017
--
-- Maintainer  :  lukyanov.georgy@gmail.com
-- Stability   :  experimental
--
-- The state of the Inglorious Adding Machine.
--
--------------------------------------------------------------------------------
module Machine.State where

import Data.SBV (Mergeable, symbolicMerge)
import Control.Monad.State.Strict
import GHC.Generics (Generic)
import Data.SBV
import Machine.Types
import Machine.Assembly

-- | The state of a Iam machine
data MachineState = MachineState
    { registers           :: RegisterBank
    , instructionCounter  :: SBV InstructionAddress
    , instructionRegister :: SBV Instruction
    , flags               :: Flags
    , memory              :: Memory
    , program             :: Program
    , clock               :: SBV Clock
    } deriving (Show, Generic, Mergeable)
