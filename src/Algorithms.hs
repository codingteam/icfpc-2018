
module Algorithms where

import Control.Monad
import Control.Monad.State
import Data.Word
import Data.Binary (decodeFile)
import Text.Printf
import qualified Data.Array.BitArray as BA
import qualified Data.Set as S

import Trace
import Sim
import Generator
import Model

data Direction = LeftToRight | RightToLeft
  deriving (Eq, Show)

firstGoodPoint :: Monad m => (P3 -> m Bool) -> [(P3, a)] -> m (Maybe a)
firstGoodPoint _ [] = return Nothing
firstGoodPoint fn ((p,a):xs) = do
  ok <- fn p
  if ok
    then return $ Just a
    else firstGoodPoint fn xs

findFreeNeighbour :: P3 -> Generator (P3, NearDiff)
findFreeNeighbour p = do
  let diff = NearDiff 0 1 0
      p' = nearPlus p diff
  return (p', diff)

-- this tries to be more clever, but does not work in general
-- findFreeNeighbour p = do
--   let diffs = [NearDiff 1 0 0, NearDiff 0 1 0, NearDiff 0 0 1,
--                NearDiff (-1) 0 0, NearDiff 0 (-1) 0, NearDiff 0 0 (-1)]
--   mbNeighbour <- firstGoodPoint isFree [(nearPlus p diff, diff) | diff <- diffs]
--   case mbNeighbour of
--     Nothing -> fail $ "Cannot find free neighbour voxel for " ++ show p
--     Just diff -> do
--       return (nearPlus p diff, diff)

fill :: BID -> Direction -> P3 -> Generator ()
fill bid dir p@(x,y,z) = do
    (neighbour, diff) <- findFreeNeighbour p
    let diff' = negateNear diff
    move bid neighbour
    grounded <- willBeGrounded p
    unless grounded $
      setHarmonics bid High
    issueFill bid diff'
    setHarmonics bid Low
    step

-- This would be too slow
-- isGrounded :: P3 -> Generator Bool
-- isGrounded p = do
--     matrix <- gets gsFilled
--     return $ go matrix S.empty p
--   where
--     go _ _ (_,0,_) = True
--     go m visited p@(x,y,z) =
--       case m BA.!? p of
--         Nothing -> return False
--         Just False -> return False
--         Just True ->
--           let neighbours = [(x+1, y, z), (x, y+1, z), (x, y, z+1),
--                             (x-1, y, z), (x, y-1, z), (x, y, z-1)]
--               nonVisited = [neighbour | neighbour `S.notMember` visited]
--           in  or [go m (S.insert neighbour visited) neighbour | neighbour <- nonVisited]

makeLine :: Direction -> Resolution -> Word8 -> Word8 -> [P3]
makeLine dir r y z = 
  case dir of
    LeftToRight -> [(x,y,z) | x <- [0..r-1]]
    RightToLeft -> [(x,y,z) | x <- reverse [0..r-1]]

selectFirstInLine :: Direction -> Word8 -> Word8 -> Generator (Maybe P3)
selectFirstInLine dir y z = do
  r <- gets (mfResolution . gsModel)
  let coords = makeLine dir r y z
  firstGoodPoint isFilledInModel $ zip coords coords

fillLine :: BID -> Direction -> Word8 -> Word8 -> Generator () 
fillLine bid dir y z = do
  mbP1 <- selectFirstInLine dir y z
  case mbP1 of
    Nothing -> -- fail $ printf "dont know where to start line: Y=%d, Z=%d" y z
      return ()
    Just p1 -> do
      r <- gets (mfResolution . gsModel)
      forM_ (makeLine dir r y z) $ \p -> do
        ok <- isFilledInModel p
        when ok $
          fill bid dir p

fillLayer :: BID -> Word8 -> Generator ()
fillLayer bid y = do
  r <- gets (mfResolution . gsModel)
  let zs = [0 .. r-1] 
      dirs = cycle [LeftToRight, RightToLeft]
  forM_ (zip zs dirs) $ \(z, dir) -> do
    fillLine bid dir y z

dumbFill :: BID -> Generator ()
dumbFill bid = do
  r <- gets (mfResolution . gsModel)
  forM_ [0 .. r-1] $ \y -> do
    fillLayer bid y
  
runTest2 :: FilePath -> Generator () -> IO ()
runTest2 path gen = do
  model <- decodeFile path
  let trace = makeTrace model gen
  print trace
  writeTrace "test2.nbt" trace

dumbHighSolver :: FilePath -> FilePath -> IO ()
dumbHighSolver modelPath tracePath = do
  model <- decodeFile modelPath
  let trace = makeTrace model $ do
                let bid = 0
                -- issueFlip bid
                dumbFill bid
                move bid (0,0,0)
                -- issueFlip bid
                issue bid Halt
  print trace
  writeTrace tracePath trace

