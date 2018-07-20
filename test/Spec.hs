{-# LANGUAGE BinaryLiterals #-}

import Test.Tasty
import Test.Tasty.HUnit

import Data.Bits
import Data.Bytes.Put
import Data.Bytes.Get
import Data.Bits.Coded
import Data.Bits.Coding

import Trace

assert :: String -> Bool -> IO ()
assert _ True = return ()
assert message False = fail message

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [specExamples]

specExamples :: TestTree
specExamples = testGroup "Specification examples"
  [
    testCase "encode.SMove.1" $
      testEncode (SMove (LongLinDiff X 12)) @?= "00010100 00011011"
  , testCase "encode.SMove.2" $
      testEncode (SMove (LongLinDiff Z (-4))) @?= "00110100 00001011"

  , testCase "encode.LMove.1" $
      testEncode (LMove (ShortLinDiff X 3) (ShortLinDiff Y (-5))) @?= "10011100 00001000"
  , testCase "encode.LMove.2" $
      testEncode (LMove (ShortLinDiff Y (-2)) (ShortLinDiff Z 2)) @?= "11101100 01110011"

  , testCase "encode.FusionP" $
      testEncode (FusionP (NearDiff (-1) 1 0)) @?= "00111111"

  , testCase "encode.FusionS" $
      testEncode (FusionS (NearDiff 1 (-1) 0)) @?= "10011110"

  , testCase "encode.Fission" $
      testEncode (Fission (NearDiff 0 0 1) 5) @?= "01110101 00000101"

  , testCase "encode.Fill" $
      testEncode (Fill (NearDiff 0 (-1) 0)) @?= "01010011"
  ]


