
module Trace where

import Data.Bits
import Data.Bytes.Put
import Data.Bytes.Get
import Data.Bits.Coded
import Data.Bits.Coding
import Data.Binary.Put (PutM)
import qualified Data.ByteString.Lazy as L
import Data.Word
import Text.Printf

data Axis = X | Y | Z
  deriving (Eq, Show, Enum)

data ShortLinDiff = ShortLinDiff Axis Int
  deriving (Eq, Show)

data LongLinDiff = LongLinDiff Axis Int
  deriving (Eq, Show)

data NearDiff = NearDiff Int Int Int
  deriving (Eq, Show)

data Command =
    Halt
  | Wait
  | Flip
  | SMove LongLinDiff
  | LMove ShortLinDiff ShortLinDiff
  | Fission NearDiff Int
  | Fill NearDiff
  | FusionP NearDiff
  | FusionS NearDiff
  deriving (Eq, Show)

instance Coded Axis where
  encode X = putBit False >> putBit True
  encode Y = putBit True >> putBit False
  encode Z = putBit True >> putBit True

  decode = do
    b1 <- getBit
    b2 <- getBit
    case (b1, b2) of
      (False, True) -> return X
      (True, False) -> return Y
      (True, True) -> return Z
      _ -> fail "Incorrect axis specification"

instance Coded ShortLinDiff where
  encode (ShortLinDiff axis n) = do
    encode axis
    putBits 3 0 n

  decode = do
    axis <- decode
    n <- getBits 3 0 0
    return $ ShortLinDiff axis n

encodeL :: Coded a => a -> L.ByteString
encodeL x = runPutL . runEncode $ encode x >> flush

encodeR :: Coding PutM () -> L.ByteString
encodeR m = runPutL . runEncode $ m >> flush

showBits :: L.ByteString -> String
showBits bstr = unwords $ map go $ L.unpack bstr
  where
    go byte = printf "%08b" byte

testEncode :: Coded a => a -> String
testEncode x = showBits $ encodeL x

testDecode :: Coded a => [Word8] -> a
testDecode bytes = runGetL (runDecode $ decode) $ L.pack bytes

