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

import Data.SBV (SBV, literal)
import Control.Monad.State (get)
import Control.Selective (handle)
import Metalanguage
import AST
import Machine.Semantics
import Machine.Semantics.Symbolic.Types
import Machine.Semantics.Symbolic.Instruction
import Machine.Semantics.Symbolic.State
import Machine.Semantics.Symbolic.Machine

buildAST :: Semantics Machine.Semantics.Monad MachineKey Value a -> Maybe (AST MachineKey Value a)
buildAST computation = computation Read Write

interpretSymbolic :: AST MachineKey Value a -> Machine a
interpretSymbolic = \case
    Read  k               -> undefined
    Write k v             -> undefined
    Fmap  func fa         -> fmap func (interpretSymbolic fa)
    Pure  x               -> pure x
    Star  ffunc fa        -> (interpretSymbolic ffunc) <*> (interpretSymbolic fa)
    Handle choice handler -> handle (interpretSymbolic choice) (interpretSymbolic handler)
    Bind  a f             -> (interpretSymbolic a) >>= (\x -> interpretSymbolic (f x))

readKey :: MachineKey -> Machine (SBV Value)
readKey = \case
    Reg  reg  -> readRegister (literal reg)
    Addr addr -> readMemory   (literal addr)
    F    flag -> readFlag     (literal flag)
    IC        -> instructionCounter <$> get
    IR        -> error "Can't read Instruction Register" -- readInstructionRegister
    Prog addr -> error "Can't read Program" -- readProgram (literal addr)