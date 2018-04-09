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
import Data.Maybe
import Data.String
import Data.Set (Set)
import qualified Data.Set as Set

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

drawGraph :: Graph (Either Key (InstructionAddress, Instruction)) -> String
drawGraph g = export style g
  where
    style = defaultStyleViaShow
        { vertexName = \v -> "v" ++ show (fromJust $ Set.lookupIndex v names)
        , vertexAttributes = \x -> case x of
            Left  k      -> [ "shape"  := "circle"
                            , "label"  := show k ]
            Right (a, i) -> [ "shape" := "record"
                            , "label" := instructionLabel a i ] }
    names = vertexSet g
    instructionLabel a i = fromString (show a <> "|" <> show i)

writeProgramGraph :: Graph (Either Key (InstructionAddress, Instruction))
                  -> FilePath -> IO ()
writeProgramGraph g dotfile =
    writeFile dotfile ((exportAsIs (fmap show g)) :: String)

data Key = Reg  Register
         | Addr MemoryAddress
         | F    Flag
         | IC
    deriving (Show, Eq, Ord)
--------------------------------------------------------------------------------
semantics :: Instruction -> Command
semantics instr = case instr of
    Halt -> Operation instr $ \read write -> pure ()
    (Load reg addr) -> Operation instr $ \read write -> do
        read (Addr addr) >>= write (Reg reg)
    (LoadMI reg addr) -> Operation instr $ \read write -> do
        addr' <- read (Addr addr)
        v <- read (Addr addr')
        write (Reg reg) v
    (Set reg simm) -> Operation instr $ \read write -> do
        write (Reg reg) simm
    (Store reg addr) -> Operation instr $ \read write -> do
        read (Reg reg) >>= write (Addr addr)
    (Add reg addr) -> Operation instr $ \read write -> do
        x <- read (Reg reg)
        y <- read (Addr addr)
        let z = x + y
        write (Reg reg) (x + y)
        if z == 0 then write (F Zero) 0 else write (F Zero) 42
    (Jump simm) -> Operation instr $ \read write -> do
        ic <- read IC
        write IC (ic + simm)
    (JumpZero simm) -> Operation instr $ \read write -> do
        zero <- read (F Zero)
        if (zero == 0) then do
            ic <- read IC
            write IC (ic + simm)
        else pure ()
--------------------------------------------------------------------------------
ex1 :: [(InstructionAddress, Instruction)]
ex1 = zip [0..]
    [ Load R0 0
    , Add  R0 1
    , Add  R0 1
--   , Load R1 2
--   , Add  R1 3
    ]

sumArray :: [(InstructionAddress, Instruction)]
sumArray = zip [0..]
    [ Load R0 0
    , Load R2 254
    , Store R2 255
    , LoadMI R1 255
    , Store R1 254
    , Add R0 254
    , Add R2 253
    , JumpZero 1
    , Jump (-7)
    , Halt
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