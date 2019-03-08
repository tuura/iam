{-# LANGUAGE ConstraintKinds,
             RankNTypes,
             FlexibleInstances #-}

module Metalanguage (
    Semantics, SemanticsV, SemanticsITE, FS
    ) where

import Prelude hiding (Read, Monad)
import Machine.Types.Value

-- | The 'Semantics' data type is a polymorphic state-transformer metalanguage
--   for describing the semantics of programming languages.
--
--   The 'Semantics' is a type constructor 'f', a constraint 'c' and two
--   effectful functions:
--              'read'  of type 'k -> f v',
--              'write' of type 'k -> f v -> f ()'
--
--   The whole thing may be though of a sort of a mutable dictionary with keys
--   of type 'k' and values of type 'v'.
--
--   Mind that the second argument of 'write' is a context-enclosed value of type 'f v'. This allows
--   the usage of constructions like 'write key2 (read key1)' without restriction
--   the type constructor 'f' to be an instance of 'Applicative' (to gain a way of
--   enclosing pure values in 'f'.)
type Semantics c k v a = forall f. c f => (k -> f v) ->
                                          (k -> f v -> f ()) ->
                                          Maybe (f a)

type Read k f g = forall a. MachineValue (g a) => k a -> f (g a)

type Write k f g = forall a. MachineValue (g a) => k a -> f (g a) -> f (g a)

type FS c k a = forall f g. (c f, Functor g) => Read k f g -> Write k f g -> f (g a)

-- | A type class for keys, equipped with an associated type family that
-- can be used to determine the type of value corresponding to the key.
class Key k where
    -- | The name of the key. Useful for avoiding heterogeneous lists of keys.
    showKey :: k a -> String

-- -- | Calculate data dependencies of a semantic computation
-- --   The computation must have only static dependencies, hence the
-- --   'Selective' constraint.
-- dependencies :: Key k => FS Selective k a -> ([String], [String])
-- dependencies task =
--     partitionEithers . getConst $
--     task trackingRead trackingWrite
--         where
--             trackingRead k    = Const [Left (showKey k)]
--             trackingWrite k fv = fv *> Const [Right (showKey k)]

type SemanticsV c k v a = forall f. c f => (k -> f v) ->
                                           (k -> f v -> f v) ->
                                           Maybe (f a)

type SemanticsITE c k v a = forall f. c f => (k -> f v) ->
                                             (k -> f v -> f ()) ->
                                             (f v -> f () -> f () -> f ()) ->
                                             Maybe (f a)