{-# LANGUAGE ConstraintKinds,
             RankNTypes #-}

module Metalanguage where

-- | The 'Semantics' data type is a polymorphic state-transformer metalanguage for describing
--   the semantics of programming languages.
--
--   The 'Semantics' is a type constructor 'f', a constraint 'c' and two monadic
--   functions: 'read'  of type 'k -> f v'
--   and        'write' of type 'k -> f v -> f ()'.
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
