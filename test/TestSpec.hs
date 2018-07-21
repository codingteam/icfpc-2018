{-# LANGUAGE BinaryLiterals #-}

module TestSpec (
  specExamples,
  codec
) where

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

specExamples :: TestTree
specExamples = testGroup "Specification examples"
  [
    testCase "encode.Halt" $
      testEncode Halt @?= "11111111"

  , testCase "encode.Wait" $
      testEncode Wait @?= "11111110"

  , testCase "encode.Flip" $
      testEncode Flip @?= "11111101"

  , testCase "encode.SMove.1" $
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

  , testCsae "encode.GFill" $
      testEncode (GFill (NearDiff 0 (-1) 0) (FarDiff 10 (-15) 20)) @?= "01010001 00101000 00001111 00110010"

  , testCase "encode.GVoid" $
      testEncode (GVoid (NearDiff 1 0 0) (FarDiff 5 5 (-5))) @?= "10110000 00100011 00100011 00011001"
  ]

testCodecFor :: (Eq a, Show a, Coded a) => a -> Assertion
testCodecFor value =
  let
    encoded = encodeL value
    decoded = runGetL (runDecode $ decode) encoded
  in decoded @?= value

codec :: TestTree
codec = testGroup "Encoding-then-decoding returns the same value"
  [
    testCase "Halt" $ testCodecFor Halt

  , testCase "Wait" $ testCodecFor Wait

  , testCase "Flip" $ testCodecFor Flip

  , testCase "SMove" $ testCodecFor (SMove (LongLinDiff X 12))

  , testCase "LMove" $
      testCodecFor (LMove (ShortLinDiff X 3) (ShortLinDiff Y (-5)))

  , testCase "FusionP" $ testCodecFor (FusionP (NearDiff (-1) 1 0))

  , testCase "FusionS" $ testCodecFor (FusionS (NearDiff 1 (-1) 0))

  , testCase "Fission" $ testCodecFor (Fission (NearDiff 0 0 1) 5)

  , testCase "Fill" $ testCodecFor (Fill (NearDiff 0 (-1) 0))

  , testCsae "GFill" $
      testCodecFor (GFill (NearDiff 0 (-1) 0) (FarDiff 10 (-15) 20))

  , testCase "GVoid" $
      testCodecFor (GVoid (NearDiff 1 0 0) (FarDiff 5 5 (-5)))
  ]
