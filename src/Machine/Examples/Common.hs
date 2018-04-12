module Machine.Examples.Common where

import qualified Data.Map as Map
import Machine.Types
import Machine.State
import Machine.Instruction
import Machine.Semantics.Dependencies
import Data.Maybe (fromJust)

emptyRegisters :: RegisterBank
emptyRegisters = Map.fromList [(R0, 0), (R1, 0), (R2, 0), (R3, 0)]

emptyFlags :: Flags
emptyFlags = Map.fromList [(Zero, 0), (Halted, 0)]

templateState :: Memory -> MachineState
templateState mem = MachineState { registers = emptyRegisters
                                 , instructionCounter = 0
                                 , instructionRegister = Jump 0
                                 , program = []
                                 , flags = emptyFlags
                                 , memory = mem
                                 , clock = 0
                                 }

ex1 :: Program -- [(InstructionAddress, Instruction)]
ex1 = zip [0..]
    [ Load R0 0
    , Add  R0 1
    -- , JumpZero 1
    -- , Add  R0 1
    ]
