import Test.Tasty

import TestSpec
import TestOptimizations

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [specExamples, codec, optimizations]
