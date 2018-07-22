module TestGenerator where

import Control.Monad.State
import Data.Array
import qualified Data.Array.BitArray as BA
import qualified Data.Map.Strict as Map
import qualified Data.Sequence as Seq

import Test.Tasty
import Test.Tasty.HUnit

import Model
import Sim
import Trace
import Generator

generator :: TestTree
generator = testGroup "Generator"
  [ testFission
  ]

emptyBitArray :: BA.BitArray P3
emptyBitArray = BA.fill (origin, origin) False

testState :: GeneratorState
testState = GS {
  gsModel = ModelFile {mfResolution = 1,
                       mfMatrix = emptyBitArray},
  gsHarmonics = Low,
  gsFilled = emptyBitArray,
  gsGrounded = emptyBitArray,
  gsStepNumber = 0,
  gsAliveBots = [],
  gsBots = Map.fromList [(0, Bot { _bid = 0, _pos = origin, _seeds = [1, 2, 3] })],
  gsTraces = array (0, maxBID) [(bid, Seq.empty) | bid <- [0 .. maxBID]]
}

testFission :: TestTree
testFission =
  let nd = NearDiff 1 0 0
      bots = Map.fromList [(0, Bot {_bid = 0
                                  ,_pos = origin
                                  ,_seeds = [3]}),
                          (1, Bot {_bid = 1
                                  ,_pos = nearPlus origin nd
                                  ,_seeds = [2]})]
      traces = gsTraces testState // [(0, Seq.fromList [Fission nd 1])]
      actualState = snd $ runState (issueFission 0 nd 1) testState
      actualBots = gsBots actualState
      actualTraces = gsTraces actualState
  in testGroup "Fission command"
    [ testCase "should add a bot with seeds transferred" $ actualBots @?= bots
    , testCase "should propely add traces" $ actualTraces @?= traces
    ]
