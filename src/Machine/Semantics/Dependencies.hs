{-# LANGUAGE RankNTypes #-}
module Machine.Semantics.Dependencies where

import Data.Either (partitionEithers)
import Data.Functor.Const

import Metalanguage

-- | Calculate data dependencies of a semantic computation
--   The computation must have only static dependencies, hence the
--   'Applicative' constraint. In case of presence of non-static dependecies
--   'Nothing' is returned.
dependencies :: Semantics Applicative k v a
             -> Maybe ([k], [k])
dependencies task =
    partitionEithers . getConst <$>
    task trackingRead trackingWrite
  where trackingRead  k    = Const [Left k]
        trackingWrite k fv = fv *> Const [Right k]

dependenciesV :: SemanticsV Applicative k v a
             -> Maybe ([k], [k])
dependenciesV task =
    partitionEithers . getConst <$>
    task trackingRead trackingWrite
  where trackingRead  k    = Const [Left k]
        trackingWrite k fv = fv *> Const [Right k]