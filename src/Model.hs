
module Model where

import Data.Array.BitArray
import Data.Array.BitArray.ByteString
import Data.Binary
import Data.Binary.Get
import Data.Binary.Put
import Data.Word
import Data.List

import Sim

data ModelFile = ModelFile {
    mfResolution :: Word8,
    mfMatrix :: BitArray P3
  }

instance Show ModelFile where
  show m = "<Model of resolution " ++ show (mfResolution m) ++ ">"

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
    intercalate "\n\n\n" [displayLayer y | y <- [0 .. r-1]]
  where
    r = mfResolution m
    matrix = mfMatrix m

    displayLayer y = unlines [displayLine y x | x <- [0 .. r-1]]

    displayLine y x = concatMap (displayVoxel y x) [0 .. r-1]

    displayVoxel y x z =
      if matrix ! (x,y,z)
        then "#"
        else "."

displayModelFile :: FilePath -> IO ()
displayModelFile path = do
  model <- decodeFile path
  putStrLn $ displayByLayer model

