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
      testEncode mkHalt @?= "11111111"

  , testCase "encode.Wait" $
      testEncode mkWait @?= "11111110"

  , testCase "encode.Flip" $
      testEncode mkFlip @?= "11111101"

  , testCase "encode.SMove.1" $
      testEncode (mkSMove (LongLinDiff X 12)) @?= "00010100 00011011"
  , testCase "encode.SMove.2" $
      testEncode (mkSMove (LongLinDiff Z (-4))) @?= "00110100 00001011"

  , testCase "encode.LMove.1" $
      testEncode (mkLMove (ShortLinDiff X 3) (ShortLinDiff Y (-5))) @?= "10011100 00001000"
  , testCase "encode.LMove.2" $
      testEncode (mkLMove (ShortLinDiff Y (-2)) (ShortLinDiff Z 2)) @?= "11101100 01110011"

  , testCase "encode.FusionP" $
      testEncode (mkFusionP (NearDiff (-1) 1 0)) @?= "00111111"

  , testCase "encode.FusionS" $
      testEncode (mkFusionS (NearDiff 1 (-1) 0)) @?= "10011110"

  , testCase "encode.Fission" $
      testEncode (mkFission (NearDiff 0 0 1) 5) @?= "01110101 00000101"

  , testCase "encode.Fill" $
      testEncode (mkFill (NearDiff 0 (-1) 0)) @?= "01010011"

  , testCase "encode.GFill" $
      testEncode (mkGFill (NearDiff 0 (-1) 0) (FarDiff 10 (-15) 20)) @?= "01010001 00101000 00001111 00110010"

  , testCase "encode.GVoid" $
      testEncode (mkGVoid (NearDiff 1 0 0) (FarDiff 5 5 (-5))) @?= "10110000 00100011 00100011 00011001"
  ]

testCodecFor :: (Eq a, Show a, Coded a) => a -> Assertion
testCodecFor value =
  let
    encoded = encodeL value
    decoded = runGetL (runDecode decode) encoded
  in decoded @?= value

codec :: TestTree
codec = testGroup "Encoding-then-decoding returns the same value"
  [
    testCase "Halt" $ testCodecFor mkHalt

  , testCase "Wait" $ testCodecFor mkWait

  , testCase "Flip" $ testCodecFor mkFlip

  , testCase "SMove" $ testCodecFor (mkSMove (LongLinDiff X 12))

  , testCase "LMove" $
      testCodecFor (mkLMove (ShortLinDiff X 3) (ShortLinDiff Y (-5)))

  , testCase "FusionP" $ testCodecFor (mkFusionP (NearDiff (-1) 1 0))

  , testCase "FusionS" $ testCodecFor (mkFusionS (NearDiff 1 (-1) 0))

  , testCase "Fission" $ testCodecFor (mkFission (NearDiff 0 0 1) 5)

  , testCase "Fill" $ testCodecFor (mkFill (NearDiff 0 (-1) 0))

  , testCase "GFill" $
      testCodecFor (mkGFill (NearDiff 0 (-1) 0) (FarDiff 10 (-15) 20))

  , testCase "GVoid" $
      testCodecFor (mkGVoid (NearDiff 1 0 0) (FarDiff 5 5 (-5)))
  ]
