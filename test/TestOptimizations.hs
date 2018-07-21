module TestOptimizations where

import Test.Tasty
import Test.Tasty.HUnit

import Optimizations
import Trace

optimizations :: TestTree
optimizations = testGroup "Optimizations" [testSplitLMove]

testSplitLMove :: TestTree
testSplitLMove = testGroup "splitLMove"
  [
    testCase "Empty input" $ splitLMove [] @?= []

  , testCase "Unoptimal input" $
      let
        a = ShortLinDiff X 3
        a' = fromShortLinDiff a
        b = ShortLinDiff Z (-1)
        b' = fromShortLinDiff b
      in splitLMove [LMove a b, Wait] @?= [SMove a', SMove b']

  , testCase "Only first occurrence is optimized" $
      let
        a = ShortLinDiff X 3
        a' = fromShortLinDiff a
        b = ShortLinDiff Z (-1)
        b' = fromShortLinDiff b
      in
        splitLMove [LMove a b, Wait, LMove a b, Wait]
        @?= [SMove a', SMove b', LMove a b, Wait]
  ]
