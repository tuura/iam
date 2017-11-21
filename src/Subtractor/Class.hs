module Subtractor.Class where

import Subtractor.Types

class Monad m => Assembly m where
    halt :: m ()
    ld :: Register -> MemoryAddress -> m ()
    ld_si :: Register -> SImm8 -> m ()
    st :: Register -> MemoryAddress -> m ()
    jmpi :: SImm10 -> m ()

