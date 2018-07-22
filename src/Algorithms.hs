
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

data LineDirection = LeftToRight | RightToLeft
  deriving (Eq, Show)

data LayerDirection = FrontToBack | BackToFront
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

fill :: BID -> P3 -> Generator ()
fill bid p@(x,y,z) = do
    (neighbour, diff) <- findFreeNeighbour p
    let diff' = negateNear diff
    move bid neighbour
    grounded <- willBeGrounded p
    unless grounded $
      setHarmonics bid High
    issueFill bid diff'

    count <- gets gsUngroundedCount
    when (count == 0) $
      setHarmonics bid Low

    step

-- This does not know yet when to switch harmonics,
-- so it should always work in High.
voidVoxel :: BID -> LineDirection -> P3 -> Generator ()
voidVoxel bid dir p = do
    (neighbour, diff) <- findFreeNeighbour p
    let diff' = negateNear diff
    move bid neighbour
    issue bid $ DoVoid diff'
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

makeLine :: LineDirection -> Resolution -> Word8 -> Word8 -> [P3]
makeLine dir r y z =
  case dir of
    LeftToRight -> [(x,y,z) | x <- [0..r-1]]
    RightToLeft -> [(x,y,z) | x <- reverse [0..r-1]]

selectFirstInLine :: LineDirection -> Word8 -> Word8 -> Generator (Maybe P3)
selectFirstInLine dir y z = do
  r <- gets (mfResolution . gsModel)
  let coords = makeLine dir r y z
  firstGoodPoint isFilledInModel $ zip coords coords

cutOne :: LongLinDiff -> (NearDiff, LongLinDiff)
cutOne (LongLinDiff X dx) = (NearDiff (signum dx) 0 0, LongLinDiff X (dx - signum dx))
cutOne (LongLinDiff Y dy) = (NearDiff 0 (signum dy) 0, LongLinDiff Y (dy - signum dy))
cutOne (LongLinDiff Z dz) = (NearDiff 0 0 (signum dz), LongLinDiff Z (dz - signum dz))

toFar :: LongLinDiff -> FarDiff
toFar (LongLinDiff X dx) = FarDiff dx 0 0
toFar (LongLinDiff Y dy) = FarDiff 0 dy 0
toFar (LongLinDiff Z dz) = FarDiff 0 0 dz

negateLong :: LongLinDiff -> LongLinDiff
negateLong (LongLinDiff x d) = LongLinDiff x (-d)

fillSegment :: BID -> LongLinDiff -> Generator ()
fillSegment bid1 ld = do
  let (nd, ld') = cutOne ld
  bid2 <- issueFission bid1 1 nd
  step
  issue bid2 $ SMove ld
  step
  issue bid1 $ GFill nd (toFar ld)
  issue bid2 $ GFill (negateNear nd) (toFar $ negateLong ld)
  step
  return ()

fillLine :: BID -> LineDirection -> Word8 -> Word8 -> Generator ()
fillLine bid dir y z = do
  mbP1 <- selectFirstInLine dir y z
  case mbP1 of
    Nothing -> -- fail $ printf "dont know where to start line: Y=%d, Z=%d" y z
      return ()
    Just p1 -> do
      r <- gets (mfResolution . gsModel)
      line <- dropWhileM (liftM not . isFilledInModel) (makeLine dir r y z)
      fillThrees bid line

fillThrees :: BID -> [P3] -> Generator ()
fillThrees _   [] = return ()
fillThrees bid [p] = do
  ok <- isFilledInModel p
  when ok $ do
    (neighbour, diff) <- findFreeNeighbour p
    let diff' = negateNear diff
    move bid neighbour
    grounded <- willBeGrounded p
    unless grounded $
      setHarmonics bid High
    issueFill bid diff'

    count <- gets gsUngroundedCount
    when (count == 0) $
      setHarmonics bid Low

    step
fillThrees bid [l, c] = do
  fillThrees bid [l]
  fillThrees bid [c]
fillThrees bid (l:c:r:line) = do
  fillThrees bid [l]
  fillThrees bid [c]
  fillThrees bid [r]

  fillThrees bid line

dropWhileM :: Monad m => (a -> m Bool) -> [a] -> m [a]
dropWhileM _ [] = return []
dropWhileM pred list@(x:xs) = do
  ok <- pred x
  if ok
    then dropWhileM pred xs
    else return list

threes :: [a] -> [[a]]
threes [] = []
threes (x:y:z:xs) = [x, y, z] : (threes xs)
threes list = [list]

voidLine :: BID -> LineDirection -> Word8 -> Word8 -> Generator ()
voidLine bid dir y z = do
  mbP1 <- selectFirstInLine dir y z
  case mbP1 of
    Nothing -> -- fail $ printf "dont know where to start line: Y=%d, Z=%d" y z
      return ()
    Just p1 -> do
      r <- gets (mfResolution . gsModel)
      forM_ (makeLine dir r y z) $ \p -> do
        ok <- isFilledInModel p
        when ok $
          voidVoxel bid dir p

fillLayer :: BID -> LayerDirection -> Word8 -> Generator ()
fillLayer bid ldir y = do
  r <- gets (mfResolution . gsModel)
  let zs = case ldir of
             FrontToBack -> [0 .. r-1]
             BackToFront -> reverse [0 .. r-1]
      dirs = cycle [LeftToRight, RightToLeft]
  forM_ (zip zs dirs) $ \(z, dir) -> do
    fillLine bid dir y z

voidLayer :: BID -> LayerDirection -> Word8 -> Generator ()
voidLayer bid ldir y = do
  r <- gets (mfResolution . gsModel)
  let zs = case ldir of
             FrontToBack -> [0 .. r-1]
             BackToFront -> reverse [0 .. r-1]
      dirs = cycle [LeftToRight, RightToLeft]
  forM_ (zip zs dirs) $ \(z, dir) -> do
    voidLine bid dir y z

dumbFill :: BID -> Generator ()
dumbFill bid = do
  r <- gets (mfResolution . gsModel)
  let ldirs = cycle [FrontToBack, BackToFront]
  forM_ (zip [0 .. r-1] ldirs) $ \(y, ldir) -> do
    fillLayer bid ldir y

dumbVoid :: BID -> Generator ()
dumbVoid bid = do
  r <- gets (mfResolution . gsModel)
  let ldirs = cycle [FrontToBack, BackToFront]
  forM_ (zip (reverse [0 .. r-1]) ldirs) $ \(y, ldir) -> do
    voidLayer bid ldir y

runTest2 :: FilePath -> Generator () -> IO ()
runTest2 path gen = do
  model <- decodeFile path
  trace <- makeTrace model gen
  print trace
  writeTrace "test2.nbt" trace

dumbHighSolver :: FilePath -> FilePath -> IO ()
dumbHighSolver modelPath tracePath = do
  model <- decodeFile modelPath
  -- printf "R: %d" (mfResolution model)
  trace <- makeTrace model $ do
                let bid = 0
                    r = mfResolution model
                dumbFill bid
                setHarmonics bid Low
                step
                bot <- getBot bid
                let (x,y,z) = _pos bot
                move bid (0,y,0)
                move bid (0,0,0)
                issue bid Halt
--   print trace
  writeTrace tracePath trace

dumbDestructor :: FilePath -> FilePath -> IO ()
dumbDestructor modelPath tracePath = do
  model <- decodeFile modelPath
  trace <- makeTrace model $ do
                let bid = 0
                r <- gets (mfResolution . gsModel)
                move bid (r-1,r-1,r-1)
                setHarmonics bid High
                step
                dumbVoid bid
                setHarmonics bid Low
                move bid (0,0,0)
                issue bid Halt
  print trace
  writeTrace tracePath trace

dumbReconstructor :: FilePath -> FilePath -> FilePath -> IO ()
dumbReconstructor srcPath dstPath tracePath = do
  srcModel <- decodeFile srcPath
  dstModel <- decodeFile dstPath
  trace <- makeTrace srcModel $ do
                let bid = 0
                r <- gets (mfResolution . gsModel)
                move bid (r-1,r-1,r-1)
                setHarmonics bid High
                dumbVoid bid
                setHarmonics bid Low
                modify $ \st -> st {gsModel = dstModel}
                let bid = 0
                dumbFill bid
                setHarmonics bid Low
                move bid (0,0,0)
                issue bid Halt
  writeTrace tracePath trace

