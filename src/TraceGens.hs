module TraceGens where

import Data.Array.BitArray
import Trace
import Sim


dummyTraceGen :: (BitArray P3) -> [Command]
dummyTraceGen matrix = [Halt]
