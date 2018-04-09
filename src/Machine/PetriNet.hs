{-# LANGUAGE ConstraintKinds, RankNTypes, ScopedTypeVariables #-}
module Machine.PetriNet where

import Prelude hiding (readIO, writeIO)
import Control.Monad.State
import Control.Monad.Writer
import qualified Data.Map.Strict as Map
import Machine.Types
import Machine.State
import Data.Either (partitionEithers)
import Algebra.Graph hiding (graph)
import Algebra.Graph.Export.Dot
import System.IO.Unsafe(unsafePerformIO)
import Data.Functor.Identity

data Operation k v a = Operation Instruction
                        (forall m. Monad m => (k -> m v) -> (k -> v -> m ()) -> m a)

type Command = Operation Key Value ()

dependencies :: forall k v m a. Monad m =>
    Operation k v a -> (k -> m v) -> (k -> v -> m ()) -> m ([k], [k])
dependencies (Operation instruction task) read write =
    (partitionEithers <$> (execWriterT $ task trackingRead trackingWrite))
  where
    trackingRead :: k -> WriterT [Either k k] m v
    trackingRead k = tell [Left k] >> lift (read k)

    trackingWrite :: k -> v -> WriterT [Either k k] m ()
    trackingWrite k v = tell [Right k] >> lift (write k v)
    -- fetch k = tell [k] >> lift (store k)

instructionGraph :: (InstructionAddress, Instruction)
                 -> (Instruction -> ([Key], [Key]))
                 -> Graph (Either Key (InstructionAddress, Instruction))
instructionGraph instrInfo@(_, instr) deps =
    let (ins, outs) = deps instr
    in overlay (star (Right instrInfo) (map Left outs))
               (transpose $ star (Right instrInfo) (map Left ins))

programGraph :: [(InstructionAddress, Instruction)]
             -> Graph (Either Key (InstructionAddress, Instruction))
programGraph prog =
    simplify . overlays $
    map (\i -> instructionGraph i deps) prog
    where deps :: Instruction -> ([Key], [Key])
          deps i = runIdentity $
            dependencies (semantics i) mockRead mockWrite

          mockRead = const . Identity $ 0
          mockWrite = const . const . Identity $ ()

writeProgramGraph :: Graph (Either Key (InstructionAddress, Instruction))
                  -> FilePath -> IO ()
writeProgramGraph g dotfile =
    writeFile dotfile ((exportAsIs (fmap show g)) :: String)

data Key = Reg  Register
         | Addr MemoryAddress
         | F    Flag
    deriving (Show, Eq, Ord)
--------------------------------------------------------------------------------
semantics :: Instruction -> Command
semantics instr = case instr of
    (Load reg addr) -> Operation instr $ \read write -> do
        v <- read (Addr addr)
        write (Reg reg) v
    (Add reg addr) -> Operation instr $ \read write -> do
        x <- read (Reg reg)
        y <- read (Addr addr)
        write (Reg reg) (x + y)

load :: Register -> MemoryAddress -> Command
load reg addr = Operation (Load reg addr) $ \read write -> do
    v <- read (Addr addr)
    write (Reg reg) v

add :: Register -> MemoryAddress -> Command
add reg addr = Operation (Add reg addr) $ \read write -> do
    x <- read (Reg reg)
    y <- read (Addr addr)
    write (Reg reg) (x + y)


--------------------------------------------------------------------------------
ex1 :: [(InstructionAddress, Instruction)]
ex1 = zip [0..]
    [ Load R0 0
    , Add  R0 1
    , Add  R0 1
--   , Load R1 2
--   , Add  R1 3
    ]
--------------------------------------------------------------------------------
readState :: Key -> State MachineState Value
readState k = do
    s <- get
    case k of Reg  reg  -> pure $ (Map.!) (registers s) reg
              Addr addr -> pure $ (Map.!) (memory s)    addr

writeState :: Key -> Value -> State MachineState ()
writeState k v = do
    s <- get
    case k of Reg reg   -> let rs' = Map.alter (const . Just $ v) reg (registers s)
                           in put $ s {registers = rs'}
              Addr addr -> let mem' = Map.alter (const . Just $ v) addr (memory s)
                           in put $ s {memory = mem'}

readIO k = do putStr (show k ++ " = "); Prelude.read <$> getLine

writeIO k v = putStrLn (show k ++ " := " ++ show v);