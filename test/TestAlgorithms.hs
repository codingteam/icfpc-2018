module TestAlgorithms where

import Test.Tasty
import Test.Tasty.HUnit

import Algorithms

algorithms :: TestTree
algorithms = testGroup "Algorithms"
  [ testThrees
  ]

testThrees :: TestTree
testThrees = testGroup "threes"
  [ testCase "Empty input" $ threes ([] :: [Int]) @?= []
  , testCase "1-element input" $ threes [1] @?= [[1]]
  , testCase "2-element input" $ threes [1, 2] @?= [[1, 2]]
  , testCase "3-element input" $ threes [1, 2, 3] @?= [[1, 2, 3]]
  , testCase "4-element input" $ threes [1..4] @?= [[1..3], [4]]
  ]
