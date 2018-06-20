{-# LANGUAGE GADTs #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  AST
-- Copyright   :  (c) Georgy Lukyanov, Andrey Mokhov 2017
--
-- Maintainer  :  mail@geo2a.info
-- Stability   :  experimental
--
-- The abstract syntactic tree encoding the monadic metalanguage.
--
--------------------------------------------------------------------------------
module AST where

import Data.SBV
import Control.Selective
import Metalanguage
import Machine.Types
import Machine.Instruction
import Machine.Semantics

data AST k v a where
    Read   :: k -> AST k v v
    Write  :: k -> AST k v a -> AST k v ()
    Fmap   :: (a -> b) -> AST k v a -> AST k v b
    Pure   :: a -> AST k v a
    Star   :: AST k v (a -> b) -> AST k v a -> AST k v b
    Handle :: AST k v (Either a b) -> AST k v (a -> b) -> AST k v b
    Bind   :: AST k v a -> (a -> AST k v b) -> AST k v b

instance Functor (AST k v) where
    fmap = Fmap

instance Applicative (AST k v) where
    pure  = Pure
    (<*>) = Star

instance Selective (AST k v) where
    handle = Handle

instance Prelude.Monad (AST k v) where
    return  = pure
    (>>=)   = Bind
