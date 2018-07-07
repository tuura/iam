{-# LANGUAGE ConstraintKinds, RankNTypes,
             ScopedTypeVariables,
             FlexibleContexts,
             FlexibleInstances,
             TypeFamilies #-}
module Machine.Examples.Energy where

import System.IO.Unsafe (unsafePerformIO)
import Control.Monad.IO.Class (liftIO)
import Data.Maybe (fromJust)
import Prelude hiding (Monad, subtract, abs)
import qualified Prelude (Monad, abs)
import Text.Pretty.Simple (pPrint)
import Data.SBV hiding (label)
import Control.Selective
import Metalanguage
import Machine.Instruction
import Machine.Semantics (semanticsM, MachineKey, executeInstruction)
import Machine.Semantics.Symbolic
import Machine.Semantics.Symbolic.Machine (run)
import Machine.Semantics.Symbolic.Types
import Machine.Semantics.Symbolic.State
import Machine.Assembly
import Machine.Value
import Machine.Examples.Common

type Monad m = (Selective m, Prelude.Monad m)
--------------------------------------------------------------------------------
energyEstimate :: Integral a => a -> a -> a -> a -> a
energyEstimate t1 t2 p1 p2 = Prelude.abs (t1 - t2) * (p1 + p2) `div` 2

energyEstimateLowLevel :: [Instruction Register MemoryAddress Flag Byte]
energyEstimateLowLevel =
    let { t1 = 0; t2 = 1; p1 = 2; p2 = 3 }
    in [ Load  R0 t1
       , Add   R0 t2
    --    , Sub   R0 t2
    --    , Abs   R0
    --    , Load  R1 p1
    --    , Add   R1 p2
    --    , Store R1 p2
    --    , Mul   R0 p2
    --    , Div   R0 255
       , Halt
       ]

theoremEnergyEstimate :: Int -> Symbolic SBool
theoremEnergyEstimate steps = do
    -- t1 <- forall "t1"
    -- t2 <- forall "t2"
    -- p1 <- forall "p1"
    -- p2 <- forall "p2"
    let t1 = 234
    let t2 = 53
    let p1 = 6
    let p2 = 8
    -- let t1 = 5190405167614263295
    -- let t2 = 5190405167614263295
    -- let p1 = 149927859193384455
    -- let p2 = 157447350457463356
    -- constrain $ t1 .>= 0 &&& t1 .<= 948672000000
    -- constrain $ t2 .>= 0 &&& t2 .<= 948672000000
    -- constrain $ p1 .>= 0 &&& p1 .<= 1000
    -- constrain $ p2 .>= 0 &&& p2 .<= 1000
    -- let summands = [1..fromIntegral n] -- replicate n (literal (1 :: Value))
    let mem = initialiseMemory [(0, t1), (1, t2), (2, p1), (3, p2), (255, 2)]
        -- steps = 1
        finalState = runModel steps $ templateState (assemble energyEstimateLowLevel) mem
        result = readArray (registers finalState) (literal R0)
    let overflow = readArray (flags finalState) (literal Overflow)
    liftIO $ putStrLn $ "R0: " ++ show (readArray (registers finalState) (literal R0))
    liftIO $ putStrLn $ "Overflow: " ++ show (readArray (flags finalState) (literal Overflow))
    liftIO $ pPrint finalState
    pure $ bnot (overflow .== 1)
       &&& result .== energyEstimate t1 t2 p1 p2 -- &&& clock finalState .< 10000
