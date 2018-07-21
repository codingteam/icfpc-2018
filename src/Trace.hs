{-# LANGUAGE BinaryLiterals #-}
{-# LANGUAGE DeriveFunctor, DeriveFoldable #-}
{-# LANGUAGE TemplateHaskell #-}

module Trace where

import Data.Bits
import Data.Bytes.Put
import Data.Bytes.Get
import Data.Bits.Coded
import Data.Bits.Coding
import Data.Binary.Get (Get)
import Data.Binary.Put (PutM)
import Control.Monad.Free
import Data.Foldable
import Data.Functor.Classes
import Data.Int
import qualified Data.ByteString.Lazy as L
import Data.Word
import Text.Printf
import Text.Show.Deriving

data Axis = X | Y | Z
  deriving (Eq, Show, Enum)

data ShortLinDiff = ShortLinDiff Axis Int8
  deriving (Eq, Show)

data LongLinDiff = LongLinDiff Axis Int8
  deriving (Eq, Show)

data NearDiff = NearDiff Int8 Int8 Int8
  deriving (Eq, Show)

data FarDiff = FarDiff Int8 Int8 Int8
  deriving (Eq, Show)

-- | CommandF is a Functor.
-- It can contain various stuff:
--  * executing bot ID
--  * global timestep index
--  * volatile cells of it
--  * energy cost
--  * a tail of following (or preceding) commands
-- etc.
data CommandF r =
    Halt r
  | Wait r
  | Flip r
  | SMove LongLinDiff r
  | LMove ShortLinDiff ShortLinDiff r
  | Fission NearDiff Word8 r
  | Fill NearDiff r
  | FusionP NearDiff r
  | FusionS NearDiff r
  | GFill NearDiff FarDiff r
  | GVoid NearDiff FarDiff r
  deriving (Eq, Show, Functor)

-- | derive Show1 instance via TH
$(deriveShow1 ''CommandF)

-- | Of course CommandF can also contain nothing, (), besides the command itself
type Command = CommandF ()

-- smart ctors
mkHalt, mkWait, mkFlip :: Command
mkHalt = Halt ()
mkWait = Wait ()
mkFlip = Flip ()
mkSMove d = SMove d ()
mkLMove d1 d2 = LMove d1 d2 ()
mkFission d i = Fission d i ()
mkFill d = Fill d ()
mkFusionP d = FusionP d ()
mkFusionS d = FusionS d ()
mkGFill nd fd = GFill nd fd ()
mkGVoid nd fd = GVoid nd fd ()

extract :: CommandF a -> a
extract (Halt r) = r
extract (Wait r) = r
extract (Flip r) = r
extract (SMove _ r) = r
extract (LMove _ _ r) = r
extract (Fission _ _ r) = r
extract (Fill _ r) = r
extract (FusionP _ r) = r
extract (FusionS _ r) = r

type CommandSeq = Free CommandF ()
cmdSeqFromList :: [Command] -> CommandSeq
cmdSeqFromList = foldr (\c t -> Free (const t <$> c)) $ Pure ()

cmdSeqToList :: CommandSeq -> [Command]
cmdSeqToList = go
  where
    go :: Free CommandF () -> [Command]
    go (Pure _) = []
    go (Free cmd) = (const () <$> cmd) : extract (go <$> cmd)


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

instance Coded FarDiff where
  encode (FarDiff dx dy dz) = do
    putWord8 (fromIntegral $ dx + 30)
    putWord8 (fromIntegral $ dy + 30)
    putWord8 (fromIntegral $ dz + 30)

  decode = do
    dx <- getWord8
    dy <- getWord8
    dz <- getWord8
    FarDiff
      <$> pure (fromIntegral dx - 30)
      <*> pure (fromIntegral dy - 30)
      <*> pure (fromIntegral dz - 30)

instance Coded r => Coded (CommandF r) where
  encode (Halt _) = putBits 7 0 (0b11111111 :: Word8)
  encode (Wait _) = putBits 7 0 (0b11111110 :: Word8)
  encode (Flip _) = putBits 7 0 (0b11111101 :: Word8)

  encode (SMove (LongLinDiff a i) _) = do
    putBits 1 0 (0b00 :: Int)
    encode a
    putBits 3 0 (0b0100 :: Int)
    putBits 2 0 (0b000 :: Int)
    putBits 4 0 (i+15)

  encode (LMove (ShortLinDiff a1 i1) (ShortLinDiff a2 i2) _) = do
    encode a2
    encode a1
    putBits 3 0 (0b1100 :: Int)
    putBits 3 0 (i2+5)
    putBits 3 0 (i1+5)

  encode (FusionP nd _) = do
    encode nd
    putBits 2 0 (0b111 :: Int)

  encode (FusionS nd _) = do
    encode nd
    putBits 2 0 (0b110 :: Int)

  encode (Fission nd m _) = do
    encode nd
    putBits 2 0 (0b101 :: Int)
    putBits 7 0 m

  encode (Fill nd _) = do
    encode nd
    putBits 2 0 (0b011 :: Int)

  encode (GFill nd fd _) = do
    encode nd
    putBits 2 0 (0b001 :: Int)
    encode fd

  encode (GVoid nd fd _) = do
    encode nd
    putBits 2 0 (0b000 :: Int)
    encode fd

  decode = parseOpcode =<< getWord8
    where
      -- parseOpcode :: (MonadGet m, Coded r) => Word8 -> Coding m (CommandF r)
      parseOpcode 0b11111111 = Halt <$> decode
      parseOpcode 0b11111110 = Wait <$> decode
      parseOpcode 0b11111101 = Flip <$> decode
      parseOpcode byte
        | byte .&. 0b11001111 == 0b00000100  = do
          {- SMove -}
          let lld_a = shift (byte .&. 0b110000) (-4)
          sndByte <- getWord8
          let lld_i = sndByte .&. 0b11111

          let lld = testDecode [shift lld_a 6 .|. shift lld_i 1]

          SMove <$> pure lld <*> decode

        | byte .&. 0b1111 == 0b1100  = do
          {- LMove -}
          let sid2_a = shift (byte .&. 0b11000000) (-6)
          let sid1_a = shift (byte .&. 0b00110000) (-4)

          sndByte <- getWord8

          let sid2_i = shift (sndByte .&. 0b11110000) (-4)
          let sid1_i = sndByte .&. 0b1111

          let sid1 = testDecode [shift sid1_a 6 .|. shift sid1_i 2]
          let sid2 = testDecode [shift sid2_a 6 .|. shift sid2_i 2]

          LMove <$> pure sid1 <*> pure sid2 <*> decode

        | byte .&. 0b111 == 0b111
          = FusionP <$> pure (testDecode [byte]) <*> decode

        | byte .&. 0b111 == 0b110
          = FusionS <$> pure (testDecode [byte]) <*> decode

        | byte .&. 0b111 == 0b101
          = Fission <$> pure (testDecode [byte]) <*> getBits 7 0 (0 :: Word8) <*> decode

        | byte .&. 0b111 == 0b011
          = Fill <$> pure (testDecode [byte]) <*> decode

        | byte .&.0b111 == 0b001 = do
          {- GFill -}
          let nd = testDecode [byte]

          dx <- getWord8
          dy <- getWord8
          dz <- getWord8
          let fd = testDecode [dx, dy, dz]

          GFill <$> pure nd <*> pure fd <*> decode

        | byte .&.0b111 == 0b000 = do
          {- GVoid -}
          let nd = testDecode [byte]

          dx <- getWord8
          dy <- getWord8
          dz <- getWord8
          let fd = testDecode [dx, dy, dz]

          GVoid <$> pure nd <*> pure fd <*> decode

        | otherwise = error "Command.decode: invalid input"

instance Coded () where
  encode () = return ()
  decode = return ()

-- | Converts short linear difference into a long linear difference.
--
-- This conversion is valid since any short linear difference satisfies
-- mlen(ld) ≤ 5, which also satisfies LongLinDiff's predicate of mlen(ld) ≤ 15.
fromShortLinDiff :: ShortLinDiff -> LongLinDiff
fromShortLinDiff (ShortLinDiff axis diff) = LongLinDiff axis diff
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
