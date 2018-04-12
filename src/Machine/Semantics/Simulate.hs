{-# LANGUAGE ConstraintKinds, RankNTypes,
             ScopedTypeVariables,
             FlexibleContexts,
             FlexibleInstances #-}
module Machine.Semantics.Simulate where

import Prelude hiding (readIO)
import Control.Monad.State
import Machine.Types
import qualified Data.Map as Map
import Metalanguage
import Machine.State
import Machine.Semantics

-- | Execute the semantics in a stateful context to get the result and the
--   final state of the microarchitecture.
simulate :: Semantics Monad k v a
         -> s
         -> (k -> State s v)
         -> (k -> State s v -> State s ())
         -> Maybe (a, s)
simulate task s0 read write =
    (flip runState) s0 <$> (task read write)
-- Example:
-- simulate (mconcat $ map semantics $ map snd ex1) defaultState readState writeState

--------------------------------------------------------------------------------

readState :: MachineKey -> State MachineState Value
readState k = do
    s <- get
    case k of Reg  reg  -> pure $ (Map.!) (registers s) reg
              Addr addr -> pure $ (Map.!) (memory s)    addr

writeState :: MachineKey -> Value -> State MachineState ()
writeState k v = do
    s <- get
    case k of Reg reg   -> let rs' = Map.alter (const . Just $ v) reg (registers s)
                           in put $ s {registers = rs'}
              Addr addr -> let mem' = Map.alter (const . Just $ v) addr (memory s)
                           in put $ s {memory = mem'}

readIO :: (Show k, Read v) => k -> IO v
readIO k = do putStr (show k ++ " = "); Prelude.read <$> getLine

writeIO :: (Show k, Show v) => k -> v -> IO ()
writeIO k v = putStrLn (show k ++ " := " ++ show v);

-- track :: Monad m => Semantics Monad k v a
--       -> (k -> m v)
--       -> (k -> v -> m ())
--       -> m ([k], [k])
-- track task read write =
--     partitionEithers <$> execWriterT (task trackingRead trackingWrite)
--   where
--     trackingRead k = tell [Left k] >> lift (read k)
--     trackingWrite k _ = tell [Right k]

-- -- Example: modular semantics: simulation with dependencies
-- trackingSimulate :: Semantics Monad k v a
--                  -> s
--                  -> (k -> State s v)
--                  -> (k -> v -> State s ())
--                  -> (([k], [k]), s)
-- trackingSimulate op s read write = runState (track op read write) s
--------------------------------------------------------------------------------
