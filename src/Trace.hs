
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
    putBits 3 0 (n+5)

  decode = do
    axis <- decode
    n <- getBits 3 0 0
    return $ ShortLinDiff axis (n-5)

instance Coded LongLinDiff where
  encode (LongLinDiff axis n) = do
    encode axis
    putBits 4 0 (n+15)

  decode = do
    axis <- decode
    n <- getBits 4 0 0
    return $ LongLinDiff axis (n-15)

instance Coded NearDiff where
  encode (NearDiff dx dy dz) = do
    let n =  (dx + 1) * 9 + (dy + 1) * 3 + (dz + 1)
    putBits 4 0 n

  decode = do
    n <- getBits 4 0 0
    let z = n `mod` 3
        xy = n `div` 3
        y = xy `mod` 3
        x = xy `div` 3
    return $ NearDiff (x-1) (y-1) (z-1)

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

