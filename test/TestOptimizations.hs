module TestOptimizations where

import Test.Tasty
import Test.Tasty.HUnit

import Optimizations
import Trace

optimizations :: TestTree
optimizations = testGroup "Optimizations"
  [ testSplitLMove
  , testMergeSMoves
  ]

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

testMergeSMoves :: TestTree
testMergeSMoves = testGroup "mergeSMoves"
  [ testCase "Empty input" $ mergeSMoves [] @?= []

  , testCase "SMoves on different axis" $
      let
        m1 = SMove (LongLinDiff X 13)
        m2 = SMove (LongLinDiff Y (-11))
      in mergeSMoves [m1, m2] @?= [m1, m2]

  , testCase "Long SMoves can't be combined" $
      let
        m1 = SMove (LongLinDiff X 13)
        m2 = SMove (LongLinDiff X 11)
      in mergeSMoves [m1, m2] @?= [m1, m2]

  , testCase "Unoptimal input.1" $
      let
        m1 = SMove (LongLinDiff X 4)
        m2 = SMove (LongLinDiff X 3)
        result = SMove (LongLinDiff X 7)
      in mergeSMoves [m1, m2] @?= [result]
  , testCase "Unoptimal input.2" $
      let
        m1 = SMove (LongLinDiff Z (-11))
        m2 = SMove (LongLinDiff Z (-4))
        result = SMove (LongLinDiff Z (-15))
      in mergeSMoves [m1, m2] @?= [result]
  , testCase "Unoptimal input.3" $
      let
        m1 = SMove (LongLinDiff Y (-3))
        m2 = SMove (LongLinDiff Y 3)
      in mergeSMoves [m1, m2] @?= []

  , testCase "Only first occurrence is optimized" $
      let
        m1 = SMove (LongLinDiff X 4)
        m2 = SMove (LongLinDiff X 3)
        combined = SMove (LongLinDiff X 7)
      in mergeSMoves [m1, m2, m1, m2] @?= [combined, m1, m2]
  ]
