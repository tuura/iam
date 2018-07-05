{-# LANGUAGE RankNTypes, GADTs, LambdaCase #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Machine.Semantics.Symbolic
-- Copyright   :  (c) Georgy Lukyanov, Andrey Mokhov 2018
--
-- Maintainer  :  mail@geo2a.info
-- Stability   :  experimental
--
-- Interpret the read-write metalanguage as symbolic execution to verify
-- programs with an SMT solver.
--------------------------------------------------------------------------------
module Machine.Semantics.Symbolic where

import Data.SBV
import Data.Maybe (fromJust)
import Control.Monad.State (get, modify)
import Control.Selective (handle)
import Metalanguage
import AST
import Machine.Semantics
import Machine.Instruction
import Machine.Assembly
import Machine.Semantics.Symbolic.Types
import Machine.Semantics.Symbolic.State
import Machine.Semantics.Symbolic.Machine

import Machine.Value (toValue, fromValue)
import Machine.Instruction.Encode
import Machine.Instruction.Decode

buildAST :: Semantics Machine.Semantics.Monad (MachineKey r addr iaddr flag) v a
         -> Maybe (AST ( MachineKey r addr iaddr flag) v a)
buildAST computation = computation Read Write

interpretSymbolic :: AST (MachineKey (SBV Register) (SBV MemoryAddress)
                                     (SBV InstructionAddress) (SBV Flag)) (SBV Value) a
                  -> Machine a
interpretSymbolic = \case
    Read  k               -> readKey k
    Write k v             -> writeKey k (interpretSymbolic v)
    Fmap  func fa         -> fmap func (interpretSymbolic fa)
    Pure  x               -> pure x
    Star  ffunc fa        -> (interpretSymbolic ffunc) <*> (interpretSymbolic fa)
    Handle choice handler -> handle (interpretSymbolic choice) (interpretSymbolic handler)
    Bind  a f             -> (interpretSymbolic a) >>= (\x -> interpretSymbolic (f x))

readKey :: MachineKey (SBV Register) (SBV MemoryAddress) (SBV InstructionAddress) (SBV Flag) -> Machine (SBV Value)
readKey = \case
    Reg  reg  -> readRegister reg
    Addr addr -> readMemory   addr
    F    flag -> readFlag     flag
    IC        -> instructionCounter <$> get
    IR        -> readInstructionRegister
    Prog addr -> readProgram addr

writeKey :: MachineKey (SBV Register) (SBV MemoryAddress) (SBV InstructionAddress) (SBV Flag)
         -> Machine (SBV Value) -> Machine ()
writeKey k v = case k of
    Reg  reg  -> v >>= writeRegister reg
    Addr addr -> v >>= writeMemory   addr
    F    flag -> v >>= writeFlag flag
    IC        -> do
        ic' <- v
        modify $ \currentState -> currentState {instructionCounter = ic'}
    IR        -> v >>= writeInstructionRegister
    Prog addr -> error "Machine.Semantics.Symbolic: Can't write Program"

assemble :: [Instruction Register MemoryAddress Flag Byte] -> Program
assemble s = foldr (\(c, p) a -> writeArray a p c) a0 (zip (map literal prg) [0..])
  where
    a0  = mkSFunArray (const $ literal 0)
    prg = map encode $ s

runModel :: Int -> MachineState -> MachineState
runModel steps state
    | steps == 0 = state
    | otherwise  = ite halted (runModel 0 state) (runModel (steps - 1) nextState)
  where
    halted    = (./= 0) $ readArray (flags state) (literal Halted)
    nextState =
        let ast = fromJust $ buildAST executeInstruction
        in snd $ run (interpretSymbolic ast) state