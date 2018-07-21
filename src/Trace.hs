{-# LANGUAGE BinaryLiterals #-}
module Trace where

import Data.Bits
import Data.Bytes.Put
import Data.Bytes.Get
import Data.Bits.Coded
import Data.Bits.Coding
import Data.Binary.Get (Get)
import Data.Binary.Put (PutM)
import Data.Int
import qualified Data.ByteString.Lazy as L
import Data.Word
import Text.Printf

data Axis = X | Y | Z
  deriving (Eq, Show, Enum)

data ShortLinDiff = ShortLinDiff Axis Int8
  deriving (Eq, Show)

data LongLinDiff = LongLinDiff Axis Int8
  deriving (Eq, Show)

data NearDiff = NearDiff Int8 Int8 Int8
  deriving (Eq, Show)

data Command =
    Halt
  | Wait
  | Flip
  | SMove LongLinDiff
  | LMove ShortLinDiff ShortLinDiff
  | Fission NearDiff Word8
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

instance Coded Command where
  encode Halt = putBits 7 0 (0b11111111 :: Word8)
  encode Wait = putBits 7 0 (0b11111110 :: Word8)
  encode Flip = putBits 7 0 (0b11111101 :: Word8)

  encode (SMove (LongLinDiff a i)) = do
    putBits 1 0 (0b00 :: Int)
    encode a
    putBits 3 0 (0b0100 :: Int)
    putBits 2 0 (0b000 :: Int)
    putBits 4 0 (i+15)

  encode (LMove (ShortLinDiff a1 i1) (ShortLinDiff a2 i2)) = do
    encode a2
    encode a1
    putBits 3 0 (0b1100 :: Int)
    putBits 3 0 (i2+5)
    putBits 3 0 (i1+5)

  encode (FusionP nd) = do
    encode nd
    putBits 2 0 (0b111 :: Int)

  encode (FusionS nd) = do
    encode nd
    putBits 2 0 (0b110 :: Int)

  encode (Fission nd m) = do
    encode nd
    putBits 2 0 (0b101 :: Int)
    putBits 7 0 m

  encode (Fill nd) = do
    encode nd
    putBits 2 0 (0b011 :: Int)

  decode = parseOpcode =<< getWord8
    where
      parseOpcode :: MonadGet m => Word8 -> Coding m Command
      parseOpcode 0b11111111 = pure Halt
      parseOpcode 0b11111110 = pure Wait
      parseOpcode 0b11111101 = pure Flip
      parseOpcode byte
        | byte .&. 0b11001111 == 0b00000100  = do
          {- SMove -}
          let lld_a = shift (byte .&. 0b110000) (-4)
          sndByte <- getWord8
          let lld_i = sndByte .&. 0b11111

          let lld = testDecode [shift lld_a 6 .|. shift lld_i 1]

          return $ SMove lld

        | byte .&. 0b1111 == 0b1100  = do
          {- LMove -}
          let sid2_a = shift (byte .&. 0b11000000) (-6)
          let sid1_a = shift (byte .&. 0b00110000) (-4)

          sndByte <- getWord8

          let sid2_i = shift (sndByte .&. 0b11110000) (-4)
          let sid1_i = sndByte .&. 0b1111

          let sid1 = testDecode [shift sid1_a 6 .|. shift sid1_i 2]
          let sid2 = testDecode [shift sid2_a 6 .|. shift sid2_i 2]

          return $ LMove sid1 sid2

        | byte .&. 0b111 == 0b111
          = FusionP <$> pure (testDecode [byte])

        | byte .&. 0b111 == 0b110
          = FusionS <$> pure (testDecode [byte])

        | byte .&. 0b111 == 0b101
          = Fission <$> pure (testDecode [byte]) <*> getBits 7 0 (0 :: Word8)

        | byte .&. 0b111 == 0b011
          = Fill <$> pure (testDecode [byte])

        | otherwise = error "Command.decode: invalid input"


encodeL :: Coded a => a -> L.ByteString
encodeL x = runPutL . runEncode $ encode x >> flush

encodeR :: Coding PutM () -> L.ByteString
encodeR m = runPutL . runEncode $ m >> flush

showBits :: L.ByteString -> String
showBits bstr = unwords $ map (printf "%08b") $ L.unpack bstr

testEncode :: Coded a => a -> String
testEncode x = showBits $ encodeL x

testDecode :: Coded a => [Word8] -> a
testDecode bytes = runGetL (runDecode decode) $ L.pack bytes

getTrace :: Coding Get [Command]
getTrace = do
  empty <- isEmpty
  if empty
    then return []
    else do
      cmd <- decode
      rest <- getTrace
      return $ cmd : rest

readTrace :: FilePath -> IO [Command]
readTrace path = do
  bstr <- L.readFile path
  return $ runGetL (runDecode getTrace) bstr

writeTrace :: FilePath -> [Command] -> IO ()
writeTrace path commands = L.writeFile path $ encodeR (encodeMany commands)
