
module Trace where

import Data.Bits
import Data.Binary
import Data.Binary.Get
import Data.Binary.Put

data Axis = X | Y | Z
  deriving (Eq, Show, Enum)

data LinDiff = LinDiff Axis Int
  deriving (Eq, Show)

data NearDiff = NearDiff Int Int Int
  deriving (Eq, Show)

data Command =
    Halt
  | Wait
  | Flip
  | SMove LinDiff
  | LMove LinDiff LinDiff
  | Fission NearDiff Int
  | Fill NearDiff
  | FusionP NearDiff
  | FusionS NearDiff
  deriving (Eq, Show)

