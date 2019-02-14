{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE StandaloneDeriving #-}

module State where

import qualified Control.Monad.State as S
import Control.Selective
import Control.Selective.Free

-- type Read k f = forall a. k a -> f a

-- type Write k f = forall a. k a -> f a -> f a

-- data FS (k :: * -> *) a where
--     Read  :: k a -> FS k a
--     Write :: k a -> FS k a -> FS k a

-- data Free f a
--   = Pure a
--   | Free (f (Free f a))

-- instance Functor f => Functor (Free f) where
--   fmap f (Pure a) = Pure (f a)
--   fmap f (Free x) = Free (fmap f <$> x)

-- instance Functor f => Applicative (Free f) where
--   pure = Pure
--   Pure f <*> Pure a = Pure (f a)
--   Pure f <*> Free x = Free (fmap f <$> x)
--   Free x <*> my     = Free ((<*> my) <$> x)

-- instance Functor f => Monad (Free f) where
--   return = pure
--   Pure a >>= f = f a
--   Free x >>= f = Free ((>>= f) <$> x)

data FS s a = FSGet (s -> a)
            | FSPut s a

deriving instance Functor (FS s)

-- fsget :: (s -> a) -> Select (FS s) a
-- fsget k = liftSelect (FSGet k)

-- fsput :: f s -> Select (FS s) ()
-- fsput s = liftSelect (FSPut s ())

data FS (k :: * -> *) a where
    Read  :: k a -> FS k a
    Write :: k a -> FS k a -> FS k a

type FSProgram s = Select (FS s)
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
data StateF s a = State (s -> (a, s))
    deriving Functor

get1 :: StateF s s
get1 = State (\s -> (s, s))

put1 :: StateF s a -> StateF s ()
put1 (State k) = State $ \s -> let (v, s') = k s
                               in ((), s')

-- instance (Show s, Show a) => Show (StateF s a) where
--     show (Get k) = "Get"
--     show (Put s v) = "Put " ++ show s

get :: Select (StateF s) s
get = liftSelect get1

put :: StateF s a -> Select (StateF s) ()
put s = liftSelect (put s)

type Program s = Select (StateF s)

stateSemantics :: StateF s a -> S.State s a
stateSemantics (State k) = S.state k

runProgram :: Program s a -> s -> (a, s)
runProgram f s =
    let semantics = runSelect stateSemantics f
    in  S.runState semantics s

-- | A minimalistic machine with two registers and one flag
type MachineState = (Int, Int, Bool)

readReg1 :: MachineState -> Int
readReg1 (reg1, _, _) = reg1

readReg2 :: MachineState -> Int
readReg2 (_, reg2, _) = reg2

readFlagZero :: MachineState -> Bool
readFlagZero (_, _, zero) = zero

writeReg1 :: MachineState -> Int -> MachineState
writeReg1 (reg1, reg2, zero) reg1' = (reg1', reg2, zero)

writeReg2 :: MachineState -> Int -> MachineState
writeReg2 (reg1, reg2, zero) reg2' = (reg1, reg2', zero)

writeFlagZero :: MachineState -> Bool -> MachineState
writeFlagZero (reg1, reg2, zero) zero' = (reg1, reg2, zero')

-- add :: Program (Int, Int, Bool) ()
-- add = let s = get id
--           x = readReg1 <$> s
--           y = readReg2 <$> s
--           sum  = (+) <$> x <*> y
--           cond = (==) <$> sum <*> pure 0
--       in -- ifS cond (put s) (put s)
--          ifS cond (put (1,2,True)) (put (2,3,False))

