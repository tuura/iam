{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE StandaloneDeriving #-}

module FS where

import qualified Control.Monad.State as S
import Control.Selective
import Control.Selective.Free
import Data.Functor.Coyoneda

-- type Read k f = forall a. k a -> f a

-- type Write k f = forall a. k a -> f a -> f a

data FS (k :: * -> *) a where
    Read  :: k a -> FS k a
    Write :: k a -> FS k a -> FS k a

data StateI s a where
    Get :: StateI s s
    Put :: s -> StateI s ()

type StateF s = Coyoneda (StateI s)

-- instance (Show k, Show v, Show a) => Show (StateF k v a) where
--     show (Get k v f) = "Get"  ++ show k ++ " " ++ show v
--     show (Put k v x) = "Put " ++ show s ++ " " ++ show v

get :: Select (StateF s) s
get = liftSelect (Coyoneda id Get)

put :: s -> Select (StateF s) ()
put s = liftSelect (Coyoneda id (Put s))

type Program s a = Select (StateF s) a

-- stateSemantics :: StateF s s -> S.State s s
-- stateSemantics (Coyoneda _ Get)     = S.get
-- stateSemantics (Coyoneda _ (Put s)) = S.put s *> pure s

stateSemantics :: StateF s s -> S.State s s
stateSemantics (Coyoneda _ Get)     = S.get
stateSemantics (Coyoneda _ (Put s)) = S.put s *> pure s

-- stateSemantics :: StateF k v a -> S.State [(k, v)] a
-- stateSemantics (Get k v a)   = S.get
-- stateSemantics (Put s v) = S.put s *> pure v

runProgram :: Program s a -> s -> s
runProgram f s =
    S.execState (runSelect stateSemantics f) s

add :: Program (Int, Int) Int
add =
    let x = fst <$> get
        y = snd <$> get
        sum = (+) <$> x <*> y
        cond = (==) <$> sum <*> pure 0
    in ifS cond (put (42, 3) *> pure 1) (put (32, 2) *> pure 0)


