{-# LANGUAGE BinaryLiterals #-}

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
main = do
  assert "encode.SMove.1" $ testEncode (SMove (LongLinDiff X 12)) == "00010100 00011011"
  assert "encode.SMove.2" $ testEncode (SMove (LongLinDiff Z (-4))) == "00110100 00001011"

  assert "encode.LMove.1" $ testEncode (LMove (ShortLinDiff X 3) (ShortLinDiff Y (-5))) == "10011100 00001000"
  assert "encode.LMove.2" $ testEncode (LMove (ShortLinDiff Y (-2)) (ShortLinDiff Z 2)) == "11101100 01110011"

  assert "encode.FusionP" $ testEncode (FusionP (NearDiff (-1) 1 0)) == "00111111"

  assert "encode.FusionS" $ testEncode (FusionS (NearDiff 1 (-1) 0)) == "10011110"

  assert "encode.Fission" $ testEncode (Fission (NearDiff 0 0 1) 5) == "01110101 00000101"

  assert "encode.Fill" $ testEncode (Fill (NearDiff 0 (-1) 0)) == "01010011"
  
