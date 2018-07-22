import Test.Tasty

import TestGenerator
import TestSpec
import TestOptimizations
import TestAlgorithms

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [generator, specExamples, codec, optimizations, algorithms]
