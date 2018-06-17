{-# LANGUAGE RankNTypes #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Machine.Types
-- Copyright   :  (c) Georgy Lukyanov, Andrey Mokhov 2017
--
-- Maintainer  :  lukyanov.georgy@gmail.com
-- Stability   :  experimental
--
-- Concurrency oracle for determining the concurrency relation between two
-- programs basing in their data dependencies. This is possible only for 
--
-----------------------------------------------------------------------------

module Machine.Semantics.Oracle (

    ) where

import Data.List (intersect)
import Metalanguage
import Machine.Semantics.Dependencies

data OracleAnswer k = Concurrent
                    | ReadConflict [k]
                    | WriteConflict [k]
                    | ReadWriteConflict [k]
    deriving (Show, Eq)

-- | Find out if two computations are data dependent by matching their
--   static dependencies
concurrencyOracle :: Eq k =>
                    Semantics Applicative k v1 a
                 -> Semantics Applicative k v2 a
                 -> Maybe (OracleAnswer k)
concurrencyOracle s1 s2 = do
    (r1, w1) <- dependencies s1
    (r2, w2) <- dependencies s2
    let readConflicts      = intersect r1 r2
        writeConflicts     = intersect w1 w2
        readWriteConflicts = intersect (r1 ++ w1) (r2 ++ w2)
    pure $ case (readConflicts, writeConflicts, readWriteConflicts) of
        ([], [], [] ) -> Concurrent
        (rs, [], rws) | rs == rws -> ReadConflict rs
        ([], ws, rws) | ws == rws -> WriteConflict ws
        (_ , _ , rws) -> ReadWriteConflict rws