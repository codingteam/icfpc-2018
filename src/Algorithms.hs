
module Algorithms where

import Control.Monad
import Control.Monad.State
import Data.Word
import Data.Maybe
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

fill :: BID -> LineDirection -> P3 -> Generator ()
fill bid dir p@(x,y,z) = do
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

data Segment = Segment Word8 Word8 Word8 -- Z, first filled X, last filled X
  deriving (Eq, Show)

checkSimpleLayer :: Word8 -> Generator (Maybe [Segment])
checkSimpleLayer y = do
    r <- gets (mfResolution . gsModel)
    go (Just []) (r-1)
  where
    go (Just []) 0 = return Nothing
    go result 0 = return result
    go (Just segs) z = do
      s <- checkSimpleLine y z
      case s of
        BadSegmenting -> return Nothing
        NoSegments -> go (Just segs) (z-1)
        SingleSegment segment -> go (Just (segment : segs)) (z-1)

data SegmentData = SegmentData {
    sdState :: SegmentCheckerState,
    sdStart :: Maybe Word8,
    sdEnd :: Maybe Word8,
    sdResult :: Bool
  }
  deriving (Eq, Show)

data SegmentCheckerState = NotStarted | Started | Ended
  deriving (Eq, Show)

data SegmentResult = SingleSegment Segment | NoSegments | BadSegmenting
  deriving (Eq, Show)

checkSimpleLine :: Word8 -> Word8 -> Generator SegmentResult
checkSimpleLine y z = do
    st <- execStateT (go 0) $ SegmentData NotStarted Nothing Nothing False
    if sdResult st
      then if sdState st == Ended
             then return $ SingleSegment $ Segment z (fromJust $ sdStart st) (fromJust $ sdEnd st)
             else return BadSegmenting
      else return NoSegments
  where
    go :: Word8 -> StateT SegmentData (StateT GeneratorState IO) ()
    go x = do
      r <- lift $ gets (mfResolution . gsModel)
      st <- get
      if x == r
        then do
            when (sdState st == Ended) $
                modify $ \st -> st {sdResult = True}
            return ()
        else do
          filled <- lift $ isFilledInModel (x, y, z)
--           lift $ lift $ printf "%d %d %d : %s\n" x y z (show filled)
          case (sdState st, filled) of
            (NotStarted, False) -> go (x+1)
            (NotStarted, True) -> do
--               lift $ lift $ printf "Started: %d %d %d\n" x y z
              modify $ \st -> st {sdStart = Just x, sdState = Started}
              go (x+1)
            (Started, True) -> go (x+1)
            (Started, False) -> do
--               lift $ lift $ printf "Ended: %d %d %d\n" x y z
              when (x <= fromJust (sdStart st) + 30) $
                  modify $ \st -> st {sdEnd = Just (x-1), sdState = Ended}
              go (x+1)
            (Ended, True) -> modify $ \st -> st {sdResult = False}
            (Ended, False) -> go (x+1)

fillSegment :: BID -> BID -> Word8 -> Segment -> Generator ()
fillSegment bid1 bid2 y (Segment z x1 x2) = do
  let nd = NearDiff 0 (-1) 0
      pos1 = (x1, y+1, z)
      pos2 = (x2, y+1, z)
      fd1 = FarDiff (fromIntegral (x2-x1)) 0 0
      fd2 = FarDiff (fromIntegral (x1-x2)) 0 0
      voxels = [(x,y,z) | x <- [x1..x2]]
  move bid2 pos2
  move bid1 pos1
--   lift $ printf "GFill: #%d @ %s, #%d @ %s\n" bid1 (show pos1) bid2 (show pos2)
  grounded <- and <$> mapM willBeGrounded voxels
  unless grounded $ do
    setHarmonics bid1 High
    step
  issue bid1 $ GFill nd fd1
  issue bid2 $ GFill nd fd2
  markFilled voxels
  forM_ voxels updateGroundedAtFill
  count <- gets gsUngroundedCount
  when (count == 0) $ do
    setHarmonics bid1 Low
    step
  step
  return ()

fillSimpleLayer :: BID -> Word8 -> [Segment] -> Generator ()
fillSimpleLayer bid1 y segments = do
  issue bid1 $ SMove $ LongLinDiff Y 1
  setBotPos bid1 $ \(x,y,z) -> (x,y+1,z)
  step
  bid2 <- issueFission bid1 1 (NearDiff 1 0 0)
  step
  forM_ segments $ \segment -> do
    fillSegment bid1 bid2 y segment
  setHarmonics bid1 Low
  step
  bot1 <- getBot bid1
  let (x1,y1,z1) = _pos bot1
  move bid2 (x1+1, y1, z1)
  issueFusion bid1 bid2
  step

fillLine :: BID -> LineDirection -> Word8 -> Word8 -> Generator ()
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
  mbSimple <- checkSimpleLayer y
  case mbSimple of
    Nothing -> do
      r <- gets (mfResolution . gsModel)
      let zs = case ldir of
                 FrontToBack -> [0 .. r-1]
                 BackToFront -> reverse [0 .. r-1]
          dirs = cycle [LeftToRight, RightToLeft]
      forM_ (zip zs dirs) $ \(z, dir) -> do
        fillLine bid dir y z
    Just segments -> do
      fillSimpleLayer bid y segments

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


runAlgorithm :: FilePath -> FilePath -> Generator () -> IO ()
runAlgorithm modelPath tracePath alrogithm = do
  model <- decodeFile modelPath
  trace <- makeTrace model $ do alrogithm
  print trace
  writeTrace tracePath trace

test3 :: IO ()
test3 = do
    model <- decodeFile "problems/FA001_tgt.mdl"
    evalStateT gen =<< initState model
  where
    gen = do
      res1 <- checkSimpleLine 1 7
      lift $ print res1
      res2 <- checkSimpleLayer 1
      lift $ print res2

test4 :: Generator ()
test4 = do
  (bid:_) <- getBids
  issue bid Flip
  step
  bid2 <- issueFission bid 1 (NearDiff 1 0 0)
  step
  let segment = Segment 1 5 15
  fillSegment bid bid2 0 segment
  move bid2 (6, 1, 1)
  issueFusion bid bid2
  step
  move bid (0,1,0)
  move bid (0,0,0)
  issue bid Flip
  issue bid Halt
  step

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

