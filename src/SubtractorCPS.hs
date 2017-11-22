{-# LANGUAGE DeriveGeneric  #-}
{-# LANGUAGE DeriveAnyClass #-}

module SubtractorCPS where

import Text.Pretty.Simple (pPrint)

import Control.Monad.IO.Class

import Data.Array (Array, Ix(..), (!), (//), array)

import Data.SBV
import Data.SBV.Tools.CodeGen
import Data.SBV.Internals (Timing(PrintTiming))

import GHC.Generics (Generic)

-- | Choose the appropriate array model to be used for modeling the memory. (See 'Memory'.)
-- The 'SFunArray' is the function based model. 'SArray' is the SMT-Lib array's based model.
type Model = SFunArray
-- type Model = SArray

-- | The memory is addressed by 8-bit words.
type Address  = SWord8

data Register = R0 | R1 | R2 | R3  deriving (Show, Eq, Ord, Ix, Bounded, Enum)

data Flag = Halted | Zero deriving (Show, Eq, Ord, Ix, Bounded, Enum)

-- | Mostek was an 8-bit machine.
type Value = SWord8

-- | Convenient synonym for symbolic machine bits.
type Bit = SBool

-- | Register bank
type Registers = Array Register Value

-- | Flag bank
type Flags = Array Flag Bit

-- | The memory maps 32-bit words to 8-bit words. (The 'Model' data-type is
-- defined later, depending on the verification model used.)
type Memory = Model Word8 Word8        -- Model defined later

data Subtractor = Subtractor { memory    :: Memory
                             , registers :: Registers
                             , flags     :: Flags
                             } deriving (Show, Generic, Mergeable)

-- | Given a machine state, compute a value out of it
type Extract a = Subtractor -> a

-- | Programs are essentially state transformers (on the machine state)
type Program = Subtractor -> Subtractor

------------------------------------------------------------------
-- * Low-level operations
------------------------------------------------------------------

-- | Get the value of a given register
getReg :: Register -> Extract Value
getReg r m = registers m ! r

-- | Set the value of a given register
setReg :: Register -> Value -> Program
setReg r v m = m {registers = registers m // [(r, v)]}

-- | Get the value of a flag
getFlag :: Flag -> Extract Bit
getFlag f m = flags m ! f

-- | Set the value of a flag
setFlag :: Flag -> Bit -> Program
setFlag f b m = m {flags = flags m // [(f, b)]}

-- | Read memory
peek :: Address -> Extract Value
peek a m = readArray (memory m) a

-- | Write to memory
poke :: Address -> Value -> Program
poke a v m = m {memory = writeArray (memory m) a v}

------------------------------------------------------------------
-- * Instruction set
------------------------------------------------------------------

-- | An instruction is modeled as a 'Program' transformer. We model
-- mostek programs in direct continuation passing style.
type Instruction = Program -> Program

-- | LDX: Set register @X@ to value @v@
ld_si :: Register -> Value -> Instruction
ld_si rX v k = k . setReg rX v

ld :: Register -> Address -> Instruction
ld rX a k m = k . setReg rX v $ m
    where v = peek a m

st :: Register -> Address -> Instruction
st rX a k m = k . poke a v $ m
    where v = getReg rX m

-- | ADC: Increment the value of register @A@ by the value of memory contents
-- at address @a@, using the carry-bit as the carry-in for the addition.
sub :: Register -> Address -> Instruction
sub rX a k m = k . setFlag Zero (v' .== 0) . setReg rX v' $ m
  where v  = peek a m
        ra = getReg rX m
        v' = ra - v -- - ra

-- | BNE: Branch if the zero-flag is false
jmpi :: Program -> Instruction
jmpi l k = const l k

-- | BNE: Branch if the zero-flag is false
jz :: Program -> Instruction
jz l k m = ite (getFlag Zero m) (l m) (k m)

-- | The 'end' combinator "stops" our program, providing the final continuation
-- that does nothing.
halt :: Program
halt = setFlag Halted true

--------------------------------------------------------------------------------

-- | Create an instance of the Mostek machine, initialized by the memory and the relevant
-- values of the registers and the flags
initMachine :: Memory -> Subtractor
initMachine mem =
    Subtractor { memory    = mem
               , registers = array (minBound, maxBound) [(R0, 0), (R1, 0), (R2, 0), (R3, 0)]
               , flags     = array (minBound, maxBound) [(Zero, false), (Halted, false)]
               }

prover = z3 { verbose = True
            , redirectVerbose = Just "example.smt2"
            , timing = PrintTiming
            , printBase = 10
            }

ex :: Program
ex = start
    where start = ld_si R0 1 $ -- halt
                  st R0 1 $
                  ld R0 0 $
                  loop
          loop  = sub R0 1 $ -- halt
                  sub R0 1 $ -- halt
                  st R0 0 $
                  jz end $
                  jmpi loop $
                  end
          end   = halt

run :: Subtractor -> Program -> Subtractor
run initialState prog =
    let finalState = prog initialState
    in finalState

initialiseMemory :: [(Address, Value)] -> Memory
initialiseMemory =
    foldr (\(a, v) m -> writeArray m a v) (mkSFunArray $ const 0)

theoremCoundown :: Symbolic SBool
theoremCoundown = do
    -- let mem = mkSFunArray (const 0)

    x <- sWord8 "x"
    -- constrain $ x .> 0
    -- constrain $ x .< 2
    -- x <- pure 4

    let mem = initialiseMemory [(0, x)]
        machine = initMachine mem
    let result = run machine ex

    liftIO $ pPrint result
    pure $ peek 0 result .== x - 1
    -- pure $ peek 0 result .== 0
    -- (getReg RegA mFinal, peek loAddr mFinal)

provingCoundown :: IO ()
provingCoundown = do
    result <- proveWith prover theoremCoundown
    print result