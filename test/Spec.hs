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
  
