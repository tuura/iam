{-# LANGUAGE GeneralizedNewtypeDeriving, RankNTypes, GADTs, LambdaCase #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Machine.Semantics.Symbolic
-- Copyright   :  (c) Georgy Lukyanov, Andrey Mokhov 2017
--
-- Maintainer  :  lukyanov.georgy@gmail.com
-- Stability   :  experimental
--
-- Interpret the metalanguage as symbolic execution and verify programs with
-- an SMT solver.
--------------------------------------------------------------------------------
module Machine.Semantics.Symbolic where

import Data.SBV (SBV, literal, mkSFunArray, writeArray, (.==))
import Control.Monad.State (get, modify)
import Control.Selective (handle)
import Metalanguage
import AST
import Machine.SemanticsNum
import Machine.Assembly
import Machine.Semantics.Symbolic.Types
import Machine.Semantics.Symbolic.Instruction
import Machine.Semantics.Symbolic.State
import Machine.Semantics.Symbolic.Machine

buildAST :: Semantics Machine.SemanticsNum.Monad (MachineKey r addr flag) v a
         -> Maybe (AST (MachineKey r addr flag) v a)
buildAST computation = computation Read Write

interpretSymbolic :: AST (MachineKey (SBV Register) (SBV MemoryAddress) (SBV Flag)) (SBV Value) a
                  -> Machine a
interpretSymbolic = \case
    Read  k               -> readKey k
    Write k v             -> writeKey k (interpretSymbolic v)
    Fmap  func fa         -> fmap func (interpretSymbolic fa)
    Pure  x               -> pure x
    Star  ffunc fa        -> (interpretSymbolic ffunc) <*> (interpretSymbolic fa)
    Handle choice handler -> handle (interpretSymbolic choice) (interpretSymbolic handler)
    Bind  a f             -> (interpretSymbolic a) >>= (\x -> interpretSymbolic (f x))

readKey :: MachineKey (SBV Register) (SBV MemoryAddress) (SBV Flag) -> Machine (SBV Value)
readKey = \case
    Reg  reg  -> readRegister reg
    Addr addr -> readMemory   addr
    F    flag -> readFlag     flag
    IC        -> instructionCounter <$> get
    -- IR        -> error "Can't read Instruction Register" -- readInstructionRegister
    -- Prog addr -> error "Can't read Program" -- readProgram (literal addr)
    -- IR        -> readInstructionRegister
    -- Prog addr -> readProgram (literal addr)

writeKey :: MachineKey (SBV Register) (SBV MemoryAddress) (SBV Flag)
         -> Machine (SBV Value) -> Machine ()
writeKey k v = case k of
    Reg  reg  -> v >>= writeRegister reg
    Addr addr -> v >>= writeMemory   addr
    F    flag -> ((.== 0) <$> v) >>= writeFlag     flag
    IC        -> do
        ic' <- v
        modify $ \currentState -> currentState {instructionCounter = ic'}
    -- IR        -> error "Can't write Instruction Register" -- readInstructionRegister
    -- Prog addr -> error "Can't write Program" -- readProgram (literal addr)

-- assemble :: Script -> Program
-- assemble s = foldr (\(c, p) a -> writeArray a p c) a0 (zip (map literal prg) [0..])
--   where
--     a0  = mkSFunArray (const $ literal Halt)
--     prg = reverse $ snd $ runWriter s []