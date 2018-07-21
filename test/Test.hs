import Test.Tasty

import TestGenerator
import TestSpec
import TestOptimizations

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [generator, specExamples, codec, optimizations]
