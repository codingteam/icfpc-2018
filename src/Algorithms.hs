
module Algorithms where

import Control.Monad
import Control.Monad.State
import Data.Word
import Data.Maybe
import Data.Binary (decodeFile)
import Text.Printf
import qualified Data.Array.BitArray as BA
import qualified Data.Array.BitArray.IO as BAIO
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

data Segment = Segment Word8 [(Word8, Word8)] -- Z, first filled X, last filled X
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

data Rectangle = Rectangle Word8 Word8 Word8 Word8 -- Z1 Z2 X1 X2
  deriving (Eq, Show)

checkRectangle :: Word8 -> Generator (Maybe Rectangle)
checkRectangle y = do
    st <- execStateT (go 0) $ LayerCheckerState NotStarted Nothing Nothing Nothing Nothing
    case lcState st of
      Ended -> do
        let first = fromJust (lcFirstLine st)
            last  = fromJust (lcLastLine st)
            left  = fromJust (lcLeft st)
            right = fromJust (lcRight st)
        return $ Just $ Rectangle first last left right
      _ -> return Nothing
  where
    go :: Word8 -> StateT LayerCheckerState (StateT GeneratorState IO) ()
    go z = do
      r <- lift $ gets (mfResolution . gsModel)
      st <- get
      if z == r
        then return ()
        else do
          line <- lift $ checkSimpleLine y z
          case (lcState st, line) of
            (NotStarted, SingleSegment (Segment z [(x1, x2)])) -> do
              modify $ \st -> st {
                  lcState = Started,
                  lcFirstLine = Just z,
                  lcLeft = Just x1,
                  lcRight = Just x2
                }
              go (z+1)
            (NotStarted, NoSegments) -> go (z+1)
            (NotStarted, BadSegmenting) ->
              modify $ \st -> st {lcState = NewSegment}
            (Started, SingleSegment (Segment z [(x1, x2)])) ->
              if lcLeft st == Just x1 && lcRight st == Just x2
                then go (z+1)
                else modify $ \st -> st {lcState = NewSegment}
            (Started, BadSegmenting) ->
              modify $ \st -> st {lcState = NewSegment}
            (Started, NoSegments) -> do
              modify $ \st -> st {lcState = Ended, lcLastLine = Just (z-1)}
              go (z+1)
            (Ended, NoSegments) -> go (z+1)
            (_, _) ->
              modify $ \st -> st {lcState = NewSegment}


data SegmentCheckerState = SegmentCheckerState {
    sdState :: SegmentState,
    sdStart :: Maybe Word8,
    sdEnd :: Maybe Word8
  }
  deriving (Eq, Show)

data LayerCheckerState = LayerCheckerState {
    lcState :: SegmentState,
    lcFirstLine :: Maybe Word8,
    lcLastLine :: Maybe Word8,
    lcLeft :: Maybe Word8,
    lcRight :: Maybe Word8
  }
  deriving (Eq, Show)

data SegmentState = NotStarted | Started | Ended | NewSegment
  deriving (Eq, Show)

data SegmentResult = SingleSegment Segment | NoSegments | BadSegmenting
  deriving (Eq, Show)

checkSimpleLine :: Word8 -> Word8 -> Generator SegmentResult
checkSimpleLine y z = do
    st <- execStateT (go 0) $ SegmentCheckerState NotStarted Nothing Nothing
    case sdState st of
      Ended ->
            let start = fromJust (sdStart st)
                end   = fromJust (sdEnd st)
                len   = end - start
            in  if len > 3
                  then case split [] start end of
                         Just subs -> return $ SingleSegment $ Segment z subs
                         Nothing -> return BadSegmenting
                  else return BadSegmenting
      NewSegment -> return BadSegmenting
      NotStarted -> return NoSegments
      s -> fail $ printf "Impossible: state at Y %d, Z %d: %s" y z (show s)
  where
    go :: Word8 -> StateT SegmentCheckerState (StateT GeneratorState IO) ()
    go x = do
      r <- lift $ gets (mfResolution . gsModel)
      st <- get
      if x == r
        then return ()
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
              modify $ \st -> st {sdEnd = Just (x-1), sdState = Ended}
              go (x+1)
            (Ended, True) -> modify $ \st -> st {sdState = NewSegment}
            (Ended, False) -> go (x+1)

    split :: [(Word8, Word8)] -> Word8 -> Word8 -> Maybe [(Word8, Word8)]
    split result start end
      | end - start < 30 = Just $ result ++ [(start, end)]
      | otherwise =
        let x1 = start + 29
        in  if end - (x1+1) > 3
              then split (result ++ [(start, x1)]) (x1+1) end
              else Nothing

fillSegment :: BID -> BID -> Word8 -> Segment -> Generator ()
fillSegment bid1 bid2 y (Segment z subsegments) = mapM_ fillSubsegment subsegments
  where
    fillSubsegment (x1, x2) = do
      let nd = NearDiff 0 (-1) 0
          pos1 = (x1, y+1, z)
          pos2 = (x2, y+1, z)
          fd1 = FarDiff (fromIntegral (x2-x1)) 0 0
          fd2 = FarDiff (fromIntegral (x1-x2)) 0 0
          voxels = [(x,y,z) | x <- [x1..x2]]
      initial pos1 pos2 x1 x2
    --   lift $ printf "GFill: #%d @ %s, #%d @ %s\n" bid1 (show pos1) bid2 (show pos2)
      grounded <- and <$> mapM willBeGrounded voxels
      unless grounded $ do
    --     lift $ printf "layer #%d, Z %d: go High\n" y z
        setHarmonics bid1 High
        step
      issue bid1 $ GFill nd fd1
      issue bid2 $ GFill nd fd2
      step
      markFilled voxels
      forM_ voxels updateGroundedAtFill

    --   r <- gets (mfResolution . gsModel)
    --   filled <- gets gsFilled
    --   matrix <- lift $ BAIO.freeze filled
    --   lift $ printf "After layer #%d, Z %d:\n%s"
    --           y z (displayLayer r matrix y)

      count <- gets gsUngroundedCount
      when (count == 0) $ do
    --     lift $ printf "layer #%d, Z %d: go Low\n" y z
        setHarmonics bid1 Low
        step
      return ()

    initial init1 init2 x1 x2 = do
      bot1 <- getBot bid1
      bot2 <- getBot bid2
      let (sx1,_,_) = _pos bot1
          (sx2,_,_) = _pos bot2
          -- if dx1 > 0 then we have to move bot1 to the right
          dx1 = fromIntegral x1 - fromIntegral sx1 :: Int
          dx2 = fromIntegral x2 - fromIntegral sx2 :: Int
      -- we know that bot1 is always at left of bot2.
      case (dx1 > 0, dx2 > 0) of
        (True, True) -> do -- move both to right. Move rightmost bot first.
          move bid2 init2
          move bid1 init1
        (False, False) -> do -- move both to left. Move leftmost bot first.
          move bid1 init1
          move bid2 init2
        _ -> do -- no matter
          move bid1 init1
          move bid2 init2

voidSegment :: BID -> BID -> Word8 -> Segment -> Generator ()
voidSegment bid1 bid2 y (Segment z subsegments) = mapM_ voidSubsegment subsegments
  where
    voidSubsegment (x1, x2) = do
      let nd = NearDiff 0 (-1) 0
          pos1 = (x1, y+1, z)
          pos2 = (x2, y+1, z)
          fd1 = FarDiff (fromIntegral (x2-x1)) 0 0
          fd2 = FarDiff (fromIntegral (x1-x2)) 0 0
          voxels = [(x,y,z) | x <- [x1..x2]]
      initial pos1 pos2 x1 x2
      issue bid1 $ GVoid nd fd1
      issue bid2 $ GVoid nd fd2
      step
      markVoid voxels

      return ()

    initial init1 init2 x1 x2 = do
      bot1 <- getBot bid1
      bot2 <- getBot bid2
      let (sx1,_,_) = _pos bot1
          (sx2,_,_) = _pos bot2
          -- if dx1 > 0 then we have to move bot1 to the right
          dx1 = fromIntegral x1 - fromIntegral sx1 :: Int
          dx2 = fromIntegral x2 - fromIntegral sx2 :: Int
      -- we know that bot1 is always at left of bot2.
      case (dx1 > 0, dx2 > 0) of
        (True, True) -> do -- move both to right. Move rightmost bot first.
          move bid2 init2
          move bid1 init1
        (False, False) -> do -- move both to left. Move leftmost bot first.
          move bid1 init1
          move bid2 init2
        _ -> do -- no matter
          move bid1 init1
          move bid2 init2

fillRectangle :: BID -> Word8 -> Rectangle -> Generator ()
fillRectangle bid1 y (Rectangle z1 z2 x1 x2) = do
  let nd = NearDiff 0 (-1) 0
      pos1 = (x1, y+1, z1)
      pos2 = (x2, y+1, z1)
      pos3 = (x1, y+1, z2)
      pos4 = (x2, y+1, z2)
      fd1 = FarDiff (fromIntegral (x2-x1)) 0 (fromIntegral (z2-z1))
      fd2 = FarDiff (fromIntegral (x1-x2)) 0 (fromIntegral (z2-z1))
      fd3 = FarDiff (fromIntegral (x2-x1)) 0 (fromIntegral (z1-z2))
      fd4 = FarDiff (fromIntegral (x1-x2)) 0 (fromIntegral (z1-z2))
      voxels = [(x,y,z) | x <- [x1..x2], z <- [z1..z2]]
  move bid1 pos1
  bid4 <- issueFission bid1 1 (NearDiff 1 0 0)
  step
  move bid4 pos4
  bid2 <- issueFission bid1 1 (NearDiff 1 0 0)
  step
  move bid2 pos2
  bid3 <- issueFission bid1 1 (NearDiff 0 0 1)
  step
  move bid3 pos3

  grounded <- and <$> mapM willBeGrounded voxels
  unless grounded $ do
--     lift $ printf "layer #%d, Z %d: go High\n" y z
    setHarmonics bid1 High
    step
  issue bid1 $ GFill nd fd1
  issue bid2 $ GFill nd fd2
  issue bid3 $ GFill nd fd3
  issue bid4 $ GFill nd fd4
  step
  markFilled voxels
  forM_ voxels updateGroundedAtFill
  count <- gets gsUngroundedCount
  when (count == 0) $ do
--     lift $ printf "layer #%d, Z %d: go Low\n" y z
    setHarmonics bid1 Low
    step
  move bid2 (x1+1, y+1, z1)
  issueFusion bid1 bid2
  step
  move bid3 (x1, y+1, z1+1)
  issueFusion bid1 bid3
  move bid4 (x1+1, y+1, z1)
  issueFusion bid1 bid4
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
--   setHarmonics bid1 Low
--   step
  bot1 <- getBot bid1
  let (x1,y1,z1) = _pos bot1
  move bid2 (x1+1, y1, z1)
  issueFusion bid1 bid2
  step

voidSimpleLayer :: BID -> Word8 -> [Segment] -> Generator ()
voidSimpleLayer bid1 y segments = do
    setHarmonics bid1 High
    step
    initial
    bid2 <- issueFission bid1 1 (NearDiff 1 0 0)
    step
    forM_ segments $ \segment -> do
      voidSegment bid1 bid2 y segment
    bot1 <- getBot bid1
    let (x1,y1,z1) = _pos bot1
    move bid2 (x1+1, y1, z1)
    issueFusion bid1 bid2
    step
    setHarmonics bid1 Low
    step
  where
    initial = do
      r <- gets (mfResolution . gsModel)
      bot1 <- getBot bid1
      let (x1,y1,z1) = _pos bot1
      when (y1 < r-1) $ do
        issue bid1 $ SMove $ LongLinDiff Y 1
        setBotPos bid1 $ \(x,y,z) -> (x,y+1,z)
        step
      when (x1 == r-1) $ do
        issue bid1 $ SMove $ LongLinDiff X (-1)
        setBotPos bid1 $ \(x,y,z) -> (x-1,y,z)
        step

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
  okL <- isFilledInModel l
  okC <- isFilledInModel c
  case (okL, okC) of
    (False, False) -> return ()
    (True, False)  -> fillThrees bid [l]
    (False, True)  -> fillThrees bid [c]
    (True, True)   -> do
      (neighbour, _) <- findFreeNeighbour l
      move bid neighbour
      forM_ [l, c] $ \p -> do
        grounded <- willBeGrounded p
        unless grounded $
          setHarmonics bid High
        let (Just diff) = nearSub p neighbour
        issueFill bid diff

        count <- gets gsUngroundedCount
        when (count == 0) $
          setHarmonics bid Low

        step
fillThrees bid (l:c:r:line) = do
  okL <- isFilledInModel l
  if not okL
    then fillThrees bid (c:r:line)
    else do
      -- invariant: l has to be filled
      okC <- isFilledInModel c
      if not okC
        then do
          fillThrees bid [l]
          fillThrees bid (r:line)
        else do
          okR <- isFilledInModel r
          if not okR
            then do
              -- If these two points aren't the last in the line, it makes
              -- sense to move to the farther so it's closer to the next
              -- segment. Thus, we rearrange points such that we should always
              -- move to the first one.
              let chunk = if (not.null) line then [c, l] else [l, c]
              fillThrees bid chunk

              fillThrees bid line
            else do
              (neighbour, _) <- findFreeNeighbour c
              move bid neighbour
              forM_ [l, c, r] $ \p -> do
                grounded <- willBeGrounded p
                unless grounded $
                  setHarmonics bid High
                let (Just diff) = nearSub p neighbour
                issueFill bid diff

                count <- gets gsUngroundedCount
                when (count == 0) $
                  setHarmonics bid Low

                step

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
      line <- dropWhileM (liftM not . isFilledInModel) (makeLine dir r y z)
      voidThrees bid line

voidThrees :: BID -> [P3] -> Generator ()
voidThrees _   [] = return ()
voidThrees bid [p] = do
  ok <- isFilledInModel p
  when ok $ do
    (neighbour, _) <- findFreeNeighbour p
    move bid neighbour
    let (Just diff) = nearSub p neighbour
    issue bid $ DoVoid diff
    step
voidThrees bid [l, c] = do
  okL <- isFilledInModel l
  okC <- isFilledInModel c
  case (okL, okC) of
    (False, False) -> return ()
    (True, False)  -> voidThrees bid [l]
    (False, True)  -> voidThrees bid [c]
    (True, True)   -> do
      (neighbour, _) <- findFreeNeighbour l
      move bid neighbour
      forM_ [l, c] $ \p -> do
        let (Just diff) = nearSub p neighbour
        issue bid $ DoVoid diff
        step
voidThrees bid (l:c:r:line) = do
  okL <- isFilledInModel l
  if not okL
    then voidThrees bid (c:r:line)
    else do
      -- invariant: l has to be filled
      okC <- isFilledInModel c
      if not okC
        then do
          voidThrees bid [l]
          voidThrees bid (r:line)
        else do
          okR <- isFilledInModel r
          if not okR
            then do
              -- If these two points aren't the last in the line, it makes
              -- sense to move to the farther so it's closer to the next
              -- segment. Thus, we rearrange points such that we should always
              -- move to the first one.
              let chunk = if (not.null) line then [c, l] else [l, c]
              voidThrees bid chunk

              voidThrees bid line
            else do
              (neighbour, _) <- findFreeNeighbour c
              move bid neighbour
              forM_ [l, c, r] $ \p -> do
                let (Just diff) = nearSub p neighbour
                issue bid $ DoVoid diff
                step

              voidThrees bid line

isLineNonEmpty :: Word8 -> Word8 -> Generator Bool
isLineNonEmpty y z = do
   first <- selectFirstInLine LeftToRight y z
   return $ isJust first

fillLayer :: BID -> LayerDirection -> Word8 -> Generator ()
fillLayer bid ldir y = do
  mbRect <- checkRectangle y
  case mbRect of
    Just rect -> do
      fillRectangle bid y rect
    Nothing -> do
      mbSimple <- checkSimpleLayer y
      case mbSimple of
        Nothing -> do
          r <- gets (mfResolution . gsModel)
          let zs = case ldir of
                     FrontToBack -> [0 .. r-1]
                     BackToFront -> reverse [0 .. r-1]
              dirs = cycle [LeftToRight, RightToLeft]
          zs' <- filterM (isLineNonEmpty y) zs
          forM_ (zip zs' dirs) $ \(z, dir) -> do
            fillLine bid dir y z
        Just segments -> do
          fillSimpleLayer bid y segments

voidLayer :: BID -> LayerDirection -> Word8 -> Generator ()
voidLayer bid ldir y = do
  setHarmonics bid High
  mbSimple <- checkSimpleLayer y
  case mbSimple of
    Nothing -> do
      r <- gets (mfResolution . gsModel)
      let zs = case ldir of
                 FrontToBack -> [0 .. r-1]
                 BackToFront -> reverse [0 .. r-1]
          dirs = cycle [LeftToRight, RightToLeft]
      zs' <- filterM (isLineNonEmpty y) zs
      forM_ (zip zs' dirs) $ \(z, dir) -> do
        voidLine bid dir y z
    Just segments -> do
      voidSimpleLayer bid y segments

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
    model <- decodeFile "problems/FA019_tgt.mdl"
    evalStateT gen =<< initState model
  where
    gen = do
      res1 <- checkSimpleLine 0 8
      lift $ print res1
      res2 <- checkSimpleLayer 0
      lift $ print res2
      res3 <- checkRectangle 0
      lift $ print res3

test4 :: Generator ()
test4 = do
  (bid:_) <- getBids
  issue bid Flip
  step
  bid2 <- issueFission bid 1 (NearDiff 1 0 0)
  step
  let segment = Segment 1 [(5, 15)]
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

