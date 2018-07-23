
module Model where

import qualified Data.Array as DA
import Data.Array.BitArray
import Data.Array.BitArray.ByteString
import Data.Binary
import Data.Binary.Get
import Data.Binary.Put
import Data.Word
import Data.List

import Sim

instance (Eq x, DA.Ix x) => Eq (BitArray x) where
  (==) a b = (elems a) == (elems b)

instance (Show x, DA.Ix x) => Show (BitArray x) where
  show = show . elems

data ModelFile = ModelFile {
    mfResolution :: Word8,
    mfMatrix :: BitArray P3
  }
  deriving (Eq)

instance Show ModelFile where
  show model =
    let res = show (mfResolution model)
    in concat ["ModelFile ", res, "³"]

getVoxel :: BitArray P3 -> P3 -> Voxel
getVoxel matrix (x,y,z) =
  if matrix ! (x,y,z)
    then Full
    else Void

bits2bytes bits | bits `mod` 8 == 0 = bits `div` 8
                | otherwise = (bits `div` 8) + 1

instance Binary ModelFile where
  get = do
    resolution <- getWord8
    let r = fromIntegral resolution :: Int
    let sz = bits2bytes $ r * r * r
    bstr <- getByteString sz
    let matrix = fromByteString ((0, 0, 0), (resolution-1, resolution-1, resolution-1)) bstr
    return $ ModelFile resolution matrix

  put m = do
    putWord8 (mfResolution m)
    let bstr = toByteString (mfMatrix m)
    putByteString bstr

displayByLayer :: ModelFile -> String
displayByLayer m =
    "Resolution : " ++ show r ++ "\n" ++
    intercalate "\n\n\n" [displayLayer r matrix y | y <- [0 .. r-1]]
  where
    r = mfResolution m
    matrix = mfMatrix m

displayLayer :: Word8 -> BitArray P3 -> Word8 -> String
displayLayer r matrix y = unlines [displayLine y x | x <- [0 .. r-1]]
  where

    displayLine y x = concatMap (displayVoxel y x) [0 .. r-1]

    displayVoxel y x z =
      if matrix ! (x,y,z)
        then "#"
        else "."

displayModelFile :: FilePath -> IO ()
displayModelFile path = do
  model <- decodeFile path
  putStrLn $ displayByLayer model

