{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Iam.State
-- Copyright   :  (c) Georgy Lukyanov, Andrey Mokhov 2017
--
-- Maintainer  :  lukyanov.georgy@gmail.com
-- Stability   :  experimental
--
-- The state of the Inglorious Adding Machine.
--
--------------------------------------------------------------------------------
module Iam.State where

import Data.SBV (Mergeable, symbolicMerge)
import Control.Monad.State.Strict
import GHC.Generics (Generic)
import Iam.Types
import Iam.Assembly

-- | The state of a Iam machine
data MachineState = MachineState
    { registers           :: RegisterBank
    , instructionCounter  :: InstructionAddress
    , instructionRegister :: Instruction
    , flags               :: Flags
    , memory              :: Memory
    , program             :: Program
    , clock               :: Clock
    } deriving (Show, Generic, Mergeable)
