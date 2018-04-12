{-# LANGUAGE ConstraintKinds,
             RankNTypes #-}

module Metalanguage where

-- | The 'Semantics' data type is a polymorphic monadic metalanguage for describing
--   the semantics of programming languages.
--
--   The 'Semantics' is a Monad and two monadic functions: 'read' of type 'k -> m v'
--   and 'write' of type  'k -> v -> m ()'.
--
--   The whole thing may be though of a sort of a mutable dictionary with keys
--   of type 'k' and values of type 'v'.
type Semantics c k v a = forall f. c f => (k -> f v) ->
                                          (k -> f v -> f ()) ->
                                          Maybe (f a)
