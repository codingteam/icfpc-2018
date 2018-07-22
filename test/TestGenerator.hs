module TestGenerator where

import Control.Monad.State
import Data.Array
import qualified Data.Array.BitArray as BA

import Test.Tasty
import Test.Tasty.HUnit

import Model
import Sim
import Trace
import Generator

generator :: TestTree
generator = testGroup "Generator"
  [ testFusion
  ]

emptyBitArray :: BA.BitArray P3
emptyBitArray = BA.fill (origin, origin) False

emptyArray :: (Ix i, Num i) => Array i x
emptyArray = listArray (0, 0) []

testState :: GeneratorState
testState = GS {
  gsModel = ModelFile {mfResolution = 0,
                       mfMatrix = emptyBitArray},
  gsHarmonics = Low,
  gsFilled = emptyBitArray,
  gsGrounded = emptyBitArray,
  gsStepNumber = 0,
  gsAliveBots = [],
  gsBots = emptyArray,
  gsTraces = emptyArray
}

testFission :: TestTree
testFission = testGroup "Fission command"
  [ let nd = NearDiff 1 0 0
        bs = Bot {_bid = 1
                 ,_pos = nearPlus origin nd
                 ,_seeds = []}
        expected = testState { gsBots = (gsBots testState) // [(1, bs)] }
        actual = snd $ runState (issueFission 0 0 nd) testState
    in testCase "should add a bot" $ actual @?= expected
  ]