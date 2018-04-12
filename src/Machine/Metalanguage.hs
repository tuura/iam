{-# LANGUAGE ConstraintKinds, RankNTypes,
             ScopedTypeVariables,
             FlexibleContexts,
             FlexibleInstances #-}
module Machine.Semantics.Graph where

import Prelude hiding (readIO, writeIO)
import Control.Monad.State
import Control.Monad.Writer
import Control.Monad.Writer.Class
import qualified Data.Map.Strict as Map
import Machine.Types
import Machine.Instruction
import Machine.State
import Data.Either (partitionEithers)
import Algebra.Graph hiding (graph)
import Algebra.Graph.Export.Dot
import System.IO.Unsafe(unsafePerformIO)
import Data.Functor.Identity
import Data.Maybe
import Data.List (nub)
import Data.Monoid
import Data.String
import Data.Set (Set)
import Data.Proxy
import qualified Data.Set as Set
import Text.Pretty.Simple (pPrint)

-- | The 'Semantics' data type is a polymorphic monadic metalanguage for describing
--   the semantics of programming languages.
--
--   The 'Semantics' is a Monad and two monadic functions: 'read' of type 'k -> m v'
--   and 'write' of type  'k -> v -> m ()'.
--
--   The whole thing may be though of a sort of a mutable dictionary with keys
--   of type 'k' and values of type 'v'.
--
--   The 'tags' field represent an auxiliary tag of some sort. May be used to
--   distinguish between two otherwise identical values.
data Semantics c tag k v a =
     Semantics { tags :: tag
               , task :: forall f. c f => (k -> f v) ->
                                          (k -> v -> f ()) ->
                                          f a
               }

instance (Monoid tags, Monoid a) => Monoid (Semantics Applicative tags k v a) where
    mempty = Semantics mempty (\read write -> pure mempty)
    mappend (Semantics tag1 task1) (Semantics tag2 task2) =
        Semantics (tag1 <> tag2)(\read write -> task1 read write *> task2 read write)

-- | Execute the semantics in a stateful context to get the result and the
--   final state of the microarchitecture.
simulate :: Semantics Monad tag k v a
         -> s
         -> (k -> State s v)
         -> (k -> v -> State s ())
         -> (a, s)
simulate (Semantics _ task) s0 readState writeState =
    runState (task readState writeState) s0
-- Example:
-- simulate (mconcat $ map semantics $ map snd ex1) defaultState readState writeState

simulate' :: [Semantics Monad tag k v a]
         -> s
         -> (k -> State s v)
         -> (k -> v -> State s ())
         -> s
simulate' s s0 readState writeState = -- (Semantics _ task) s0 readState writeState =
    snd $ (flip runState  s0) . sequence $ map (\(Semantics _ task) -> task readState writeState) s

track :: Monad m => Semantics Monad tag k v a
      -> (k -> m v)
      -> (k -> v -> m ())
      -> m ([k], [k])
track (Semantics _ task) read write =
    partitionEithers <$> execWriterT (task trackingRead trackingWrite)
  where
    trackingRead k = tell [Left k] >> lift (read k)
    trackingWrite k _ = tell [Right k]

-- Example: modular semantics: simulation with dependencies
trackingSimulate :: Semantics Monad tag k v a
                 -> s
                 -> (k -> State s v)
                 -> (k -> v -> State s ())
                 -> (([k], [k]), s)
trackingSimulate op s read write = runState (track op read write) s
--------------------------------------------------------------------------------
data Key = Reg  Register
         | Addr MemoryAddress
         | F    Flag
         | IC
         | IR
         | Prog InstructionAddress
    deriving (Show, Eq, Ord)

-- | Semantics of IAM instructions encoded in terms of the 'Semantics'
--   polymorphic monadic metalanguage.
--
--   The resulting 'Semantic' values are tagged with a singleton list
--   containing a corresponding 'Instruction'.
instructionSemantics :: Instruction -> Semantics Monad [Instruction] Key Value ()
instructionSemantics instr = Semantics [instr] (execute instr)
    where execute Halt _ write =
            write (F Halted) 1 *>
            pure ()
          execute (Load reg addr) read write =
            read (Addr addr) >>= write (Reg reg)
          execute (LoadMI reg addr) read write = do
            addr' <- read (Addr addr)
            v <- read (Addr addr')
            write (Reg reg) v
          execute (Set reg simm) read write =
            write (Reg reg) simm
          execute (Store reg addr) read write =
            read (Reg reg) >>= write (Addr addr)
          execute (Add reg addr) read write = do
            x <- read (Reg reg)
            y <- read (Addr addr)
            let z = x + y
            write (Reg reg) (x + y)
            -- write (Reg reg) ((+) <$> read (Reg reg) <*> read )
            if z == 0 then write (F Zero) 1 else write (F Zero) 0
          execute (Jump simm) read write = do
            ic <- read IC
            write IC (ic + simm)
          execute (JumpZero simm) read write = do
            zero <- read (F Zero)
            if (zero == 1) then do
                ic <- read IC
                write IC (ic + simm)
            else pure ()

instructionSemantics' :: Instruction -> Semantics c [Instruction] Key Value ()
instructionSemantics' = undefined

-- decodeInstruction :: Value -> Instruction
-- decodeInstruction = undefined

-- encodeInstruction :: Instruction -> Value
-- encodeInstruction = undefined

-- incrementInstructionCounter :: Semantics [Instruction] Key Value ()
-- incrementInstructionCounter = Semantics [] $ \read write -> do
--     ic <- read IC
--     write IC (ic + 1)

-- readInstructionRegister :: Semantics [Instruction] Key Value Instruction
-- readInstructionRegister = Semantics [] $ \read _ ->
--     decodeInstruction <$> read IR

-- writeInstructionRegister :: Instruction -> Semantics [Instruction] Key Value ()
-- writeInstructionRegister instr = Semantics [] $ \_ write ->
--     write IR (encodeInstruction instr)

-- --------------------------------------------------------------------------------
-- trackInstruction :: Instruction
--                  -> State MachineState ([Key], [Key])
-- trackInstruction i = track (instructionSemantics i) readState writeState

-- instructionDataGraph :: (InstructionAddress, Instruction)
--                      -> State MachineState (Graph (Either Key (InstructionAddress, Instruction)))
-- instructionDataGraph instrInfo@(_, instr) = do
--     (ins, outs) <- trackInstruction instr
--     pure $ overlay (star (Right instrInfo) (map Left outs))
--                    (transpose $ star (Right instrInfo) (map Left ins))

-- -- programDataGraph :: Program -- [Semantics tag k v a]
-- --          -> s
-- --          -> (k -> State s v)
-- --          -> (k -> v -> State s ())
-- --          -> s
-- -- programDataGraph s s0 readState writeState = -- (Semantics _ task) s0 readState writeState =
-- --     snd $ (flip runState  s0) . sequence $ map (\(Semantics _ task) -> task readState writeState) s


-- -- programDataGraph :: Program
-- --                  -> Graph (Either Key (InstructionAddress, Instruction))
-- programDataGraph p = eval (empty, defaultState) (map instructionDataGraph p)
--         where 
--             --   eval :: (Graph a, s) -> [State s (Graph a)] -> Graph a
--             --   eval (g, _) [] = g
--             --   eval (g, s) (a:as) = let acc@(gNext, s') = runState a s
--             --                        in overlay gNext $ eval acc as
--               eval = scanl go
--               go (g, s) a = let (g', s') = runState a s
--                             in  (overlay g' g, s')

-- ------------------------------------------
-- ex1 :: Program -- [(InstructionAddress, Instruction)]
-- ex1 = zip [0..]
--     [ Load R0 0
--     , Add  R0 1
--     , JumpZero 1
--     , Add  R0 1
-- --   , Load R1 2
-- --   , Add  R1 3
--     ]

-- sumArray :: [(InstructionAddress, Instruction)]
-- sumArray = zip [0..]
--     [ Load R0 0
--     -- , Jump 10
--     , Load R2 254
--     , Store R2 255
--     , LoadMI R1 255
--     , Store R1 254
--     , Add R0 254
--     , Add R2 253
--     -- , Add R3 42
--     , JumpZero 1
--     , Jump (-7)
--     , Halt
--     ]
-- --------------------------------------------------------------------------------
-- data IsLeader = Yes | No

-- leaders :: Program -> [InstructionAddress]
-- leaders p = nub $ initInstr p : branchTargets p ++ branchFollowers p
--     where initInstr :: Program -> InstructionAddress
--           initInstr = fst . head

--           branchTargets :: Program
--                         -> [InstructionAddress]
--           branchTargets = foldl mark []
--               where mark acc (addr, instr) =
--                          case instr of
--                               Jump simm -> (addr + simm) : acc
--                               JumpZero simm -> (addr + simm) : acc
--                               _ -> acc

--           branchFollowers :: Program -> [InstructionAddress]
--           branchFollowers p = foldl go [] $ zip p (tail p ++ [head p])
--               where go acc (p, q) =
--                        case snd p of
--                               Jump _ -> fst q : acc
--                               JumpZero _ -> fst q : acc
--                               _ -> acc
-- --------------------------------------------------------------------------------
-- readIO :: (Show k, Read v) => k -> IO v
-- readIO k = do putStr (show k ++ " = "); Prelude.read <$> getLine

-- writeIO :: (Show k, Show v) => k -> v -> IO ()
-- writeIO k v = putStrLn (show k ++ " := " ++ show v);


-- readState :: Key -> State MachineState Value
-- readState k = do
--     s <- get
--     -- k <- lift (read k')
--     case k of Reg  reg  -> pure $ (Map.!) (registers s) reg
--               Addr addr -> pure $ (Map.!) (memory    s) addr
--               F    flag -> pure $ (Map.!) (flags     s) flag
--               IC        -> pure (instructionCounter s)

-- writeState :: Key -> Value -> State MachineState ()
-- writeState k v = do
--     s <- get
--     case k of Reg reg   -> let rs' = Map.alter (const . Just $ v) reg (registers s)
--                            in  put $ s {registers = rs'}
--               Addr addr -> let mem' = Map.alter (const . Just $ v) addr (memory s)
--                            in  put $ s {memory = mem'}
--               F    flag -> let flags' = Map.alter (const . Just $ v) flag (flags s)
--                            in  put $ s {flags = flags'}
--               IC        -> put $ s {instructionCounter = v}
-- --------------------------------------------------------------------------------
-- defaultState :: MachineState
-- defaultState = MachineState { registers = Map.fromList [(R0, 0), (R1, 0), (R2, 0), (R3, 0)]
--                             , instructionCounter = 0
--                             , instructionRegister = Jump 0
--                             , program = []
--                             , flags = Map.fromList [(Zero, 0), (Halted, 0)]
--                             -- , memory = Map.fromList $ zip [0..255] [0..]
--                             , memory = Map.fromList $ [(0, 0)]
--                                        ++ zip [1..] [1..10]
--                                        ++ zip [11..252] [0,0..]
--                                        ++ [ (253, -1)
--                                           , (254, 3)
--                                           , (255, 0)
--                                           ]
--                             , clock = 0
--                             }
-- --------------------------------------------------------------------------------
-- drawGraph :: Graph (Either Key (InstructionAddress, Instruction)) -> String
-- drawGraph g = export style g
--   where
--     style = defaultStyleViaShow
--         { vertexName = \v -> "v" ++ show (fromJust $ Set.lookupIndex v names)
--         , vertexAttributes = \x -> case x of
--             Left  k      -> [ "shape"  := "circle"
--                             , "label"  := show k ]
--             Right (a, i) -> [ "shape" := "record"
--                             , "label" := instructionLabel a i ] }
--     names = vertexSet g
--     instructionLabel a i = fromString (show a <> "|" <> show i)