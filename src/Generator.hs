
module Generator where

import Control.Monad
import Control.Monad.State
import Data.Maybe
import Data.Array
import Data.Int
import Data.List
import Data.Ord
import qualified Data.Sequence as Seq
import qualified Data.Set as S
import Data.Sequence ((|>))
import qualified Data.Array.BitArray as BA
import qualified Data.Array.BitArray.IO as BAIO

import Trace
import Sim
import Model

{-
 - We may have several bots, and commands to these bots are to be issued sort of "in parallel":
 -
 - Bot 1: C C C _ _ C C C C ...
 - Bot 2:   C C C C _ _ C C ...
 - Bot 3:     C _ C C _ C C ...
 -
 - where _ stands for Wait and C stands for some other command.
 - Initially we have just one bot, other start working later.
 - Number of bots can also be decreased sometimes.
 -}

type BotTrace = Seq.Seq Command

type Step = Int

type AliveBot = BID

-- | Offset relative to some point in the matrix.
type P3d = (Int16, Int16, Int16)

data GeneratorState = GS {
    gsModel :: ! ModelFile,
    gsHarmonics :: Harmonics,
    gsFilled :: ! (BA.BitArray P3), -- voxels that are already filled by generator
    gsGrounded :: ! (BAIO.IOBitArray P3), -- grounded voxels
    gsStepNumber :: Step,
    gsAliveBots :: [(Step, [AliveBot])], -- which bots are alive. Record is to be added when set of bots is changed.
    gsBots :: Array BID BotState,
    gsTraces :: ! (Array BID BotTrace) -- Trace is to be filled with Wait if bot does nothing or is not alive.
  }
  deriving (Eq, Show)

maxBID :: BID
maxBID = 20

initState :: ModelFile -> IO GeneratorState
initState model = do
    grounded <- BAIO.newArray_ ((0,0,0), (r-1,r-1,r-1))
    return $ GS model Low filled grounded 0 [(0,[bid])] bots traces
  where
    bid = 0
    bots   = array (0, maxBID) [(bid, Bot bid (0,0,0) []) | bid <- [0 .. maxBID]]
    traces = array (0, maxBID) [(bid, Seq.empty) | bid <- [0 .. maxBID]]
    r = mfResolution model
    filled = BA.array ((0,0,0), (r-1,r-1,r-1)) [((x,y,z), False) | x <- [0..r-1], y <- [0..r-1], z <- [0..r-1]]

type Generator a = StateT GeneratorState IO a

getBotTrace :: BID -> Generator BotTrace
getBotTrace bid = do
  traces <- gets gsTraces
  return $ traces ! bid

-- Check that there is such bot
checkBid :: BID -> Generator ()
checkBid bid = do
  bids <- gets (indices . gsBots)
  when (bid `notElem` bids) $
      fail $ "There is currently no such bot: " ++ show bid ++ "\nCurrent bots are:\n" ++ show bids

getBids :: Generator [BID]
getBids = do
  gets (indices . gsBots)

getBot :: BID -> Generator BotState
getBot bid = do
  bots <- gets gsBots
  return $ bots ! bid

-- | Issue one command for one bot.
issue :: BID -> Command -> Generator ()
issue bid cmd = do
  checkBid bid
  trace <- getBotTrace bid
  let trace' = trace |> cmd
  modify $ \st -> st {gsTraces = gsTraces st // [(bid, trace')]}

flipH :: Harmonics -> Harmonics
flipH Low = High
flipH High = Low

-- | Issue Flip command and remember resulting harmonics.
issueFlip :: BID -> Generator ()
issueFlip bid = do
  modify $ \st -> st {gsHarmonics = flipH (gsHarmonics st)}
  issue bid Flip

-- | Issue Fission command forking a new bot taking N seeds with him from the current bot.
issueFission :: BID -> Int -> NearDiff -> Generator ()
issueFission bid n direction = do
  modify $ \st -> st

-- | Set harmonics to target value
setHarmonics :: BID -> Harmonics -> Generator ()
setHarmonics bid target = do
  current <- gets gsHarmonics
  when (current /= target) $
      issueFlip bid

nearPlus :: P3 -> NearDiff -> P3
nearPlus (x,y,z) (NearDiff dx dy dz) = (x+fromIntegral dx, y+fromIntegral dy, z+fromIntegral dz)

negateNear :: NearDiff -> NearDiff
negateNear (NearDiff dx dy dz) = NearDiff (-dx) (-dy) (-dz)

-- | Issue the Fill command
-- This will mark the voxel as filled in generator's state
-- Returns True if as a result the voxel is grounded.
issueFill :: BID -> NearDiff -> Generator Bool
issueFill bid nd = do
    bot <- getBot bid
    let c' = nearPlus (_pos bot) nd
    filled <- isFilled c'
    if filled
      then fail $ "Voxel is already filled: " ++ show c'
      else do
           issue bid $ Fill nd
           modify $ \st -> st {gsFilled = gsFilled st BA.// [(c', True)]}
           updateGrounded c'
  where
    updateGrounded :: P3 -> Generator Bool
    updateGrounded p@(x,y,z) = do
        grounded <- gets gsGrounded
        result <- check grounded p

        groundedHelper S.empty [p]

        return result

    groundedHelper :: S.Set P3 -> [P3] -> Generator ()
    groundedHelper _       [] = return ()
    groundedHelper checked (p@(x,y,z) : toCheck) = do
        filled <- isFilled p

        grounded <- gets gsGrounded
        isGrounded <- check grounded p

        let checked' = S.insert p checked

        if filled && not isGrounded
          then do
            setGrounded grounded p True

            resolution <- liftM mfResolution $ gets gsModel
            let inBounds x = x >= 0 && x < resolution

            let neighbours =
                  filter (\x -> S.notMember x checked') $
                  filter (\(x, y, z) -> inBounds x && inBounds y && inBounds z)
                      [(x+1, y, z), (x, y+1, z), (x, y, z+1),
                       (x-1, y, z), (x, y-1, z), (x, y, z-1)]
            groundedHelper checked' (neighbours ++ toCheck)
          else
            -- p is either filled && grounded, or not filled && not grounded.
            -- Either way, it shouldn't be grounded, so its neighbours don't
            -- need to be checked and updated.
            groundedHelper checked' toCheck

    setGrounded :: BAIO.IOBitArray P3 -> P3 -> Bool -> Generator ()
    setGrounded bits p ok =
      lift $ BAIO.writeArray bits p ok

    -- we just filled this voxel by issuing Fill command
    check :: BAIO.IOBitArray P3 -> P3 -> Generator Bool
    check _ (_,0,_) = return True
    check grounded p@(x,y,z) = do
          resolution <- liftM mfResolution $ gets gsModel
          let inBounds x = x >= 0 && x < resolution

          let neighbours =
                filter (\(x, y, z) -> inBounds x && inBounds y && inBounds z)
                    [(x+1, y, z), (x, y+1, z), (x, y, z+1),
                     (x-1, y, z), (x, y-1, z), (x, y, z-1)]
          neighbGrounded <- forM neighbours $ \n ->
                                lift $ BAIO.readArray grounded n
          return $ or neighbGrounded

-- | Is voxel grounded?
-- This works by definition, i.e. always returns False for non-filled voxels.
isGrounded :: P3 -> Generator Bool
isGrounded p = do
  grounded <- gets gsGrounded
  lift $ BAIO.readArray grounded p

-- | Will voxel become grounded if we fill it?
-- This checks if any neighbour voxel is grounded.
willBeGrounded :: P3 -> Generator Bool
willBeGrounded (x,y,z) = do
  grounded <- gets gsGrounded
  let neighbours = [(x+1, y, z), (x, y+1, z), (x, y, z+1),
                    (x-1, y, z), (x, y-1, z), (x, y, z-1)]
  r <- gets (mfResolution . gsModel)
  let good (nx,ny,nz) =
        nx >= 0 && nx < r && ny >= 0 && ny < r && nz >= 0 && nz < r
  neighbGrounded <- forM neighbours $ \n ->
                        if good n
                          then lift $ BAIO.readArray grounded n
                          else return False
  return $ or neighbGrounded

allAreGrounded :: Generator Bool
allAreGrounded = do
  filledMatrix <- gets gsFilled
  let filledIdxs = [idx | idx <- BA.indices filledMatrix, filledMatrix BA.! idx]
  grounded <- gets gsGrounded
  res <- forM filledIdxs $ \p ->
             lift $ BAIO.readArray grounded p
  return $ and res

-- | Switch to the next step.
-- If we did not issue commands for some bots on current steps,
-- automatically issue Wait command for them.
step :: Generator ()
step = do
    n <- gets gsStepNumber
    let n' = n+1
    traces <- gets gsTraces
    let updates = mapMaybe update (indices traces)
        update bid = let trace = traces ! bid
                     in  if length trace < n'
                           then Just (bid, trace |> Wait)
                           else Nothing
    let traces' = traces // updates
    modify $ \st -> st {gsStepNumber = n', gsTraces = traces'}

subtractCmd :: P3d -> Command -> P3d
subtractCmd (dx, dy, dz) (SMove (LongLinDiff X dx1)) = (dx-(fromIntegral dx1), dy, dz)
subtractCmd (dx, dy, dz) (SMove (LongLinDiff Y dy1)) = (dx, dy-(fromIntegral dy1), dz)
subtractCmd (dx, dy, dz) (SMove (LongLinDiff Z dz1)) = (dx, dy, dz-(fromIntegral dz1))
subtractCmd (dx, dy, dz) (LMove (ShortLinDiff X dx1) (ShortLinDiff X dx2)) = (dx-(fromIntegral dx1)-(fromIntegral dx2), dy, dz)
subtractCmd (dx, dy, dz) (LMove (ShortLinDiff Y dy1) (ShortLinDiff Y dy2)) = (dx, dy-(fromIntegral dy1)-(fromIntegral dy2), dz)
subtractCmd (dx, dy, dz) (LMove (ShortLinDiff Z dz1) (ShortLinDiff Z dz2)) = (dx, dy, dz-(fromIntegral dz1)-(fromIntegral dz2))
subtractCmd (dx, dy, dz) (LMove (ShortLinDiff X dx1) (ShortLinDiff Y dy2)) = (dx-(fromIntegral dx1), dy-(fromIntegral dy2), dz)
subtractCmd (dx, dy, dz) (LMove (ShortLinDiff X dx1) (ShortLinDiff Z dz2)) = (dx-(fromIntegral dx1), dy, dz-(fromIntegral dz2))
subtractCmd (dx, dy, dz) (LMove (ShortLinDiff Y dy1) (ShortLinDiff Z dz2)) = (dx, dy-(fromIntegral dy1), dz-(fromIntegral dz2))
subtractCmd p (LMove sld1 sld2) = subtractCmd p (LMove sld2 sld1)
subtractCmd _ c = error $ "Impossible move command: " ++ show c

origin :: P3
origin = (0,0,0)

clamp :: Ord a => (a,  a) -> a -> a
clamp (low, high) value = max low (min value high)

plus :: P3d -> P3d -> P3d
plus (x1,y1,z1) (x2,y2,z2) = (x1+y1, x2+y2, z1+z2)

extractMove :: P3d -> Either P3d (Command, P3d)
extractMove p@(dx, dy, dz) =
  let clamp5 = clamp (-5, 5)
      clamp15 = clamp (-15, 15)
  in case (dx /= 0, dy /= 0, dz /= 0) of
    (True, True, False) ->
      let cmd = LMove (ShortLinDiff X (fromIntegral $ clamp5 dx)) (ShortLinDiff Y (fromIntegral $ clamp5 dy))
          res = subtractCmd p cmd
      in  Right (cmd, res)
    (True, False, True) ->
      let cmd = LMove (ShortLinDiff X (fromIntegral $ clamp5 dx)) (ShortLinDiff Z (fromIntegral $ clamp5 dz))
          res = subtractCmd p cmd
      in  Right (cmd, res)
    (False, True, True) ->
      let cmd = LMove (ShortLinDiff Y (fromIntegral $ clamp5 dy)) (ShortLinDiff Z (fromIntegral $ clamp5 dz))
          res = subtractCmd p cmd
      in  Right (cmd, res)
    (True, False, False) ->
      let cmd = SMove (LongLinDiff X $ fromIntegral $ clamp15 dx)
          res = subtractCmd p cmd
      in  Right (cmd, res)
    (False, True, False) ->
      let cmd = SMove (LongLinDiff Y $ fromIntegral $ clamp15 dy)
          res = subtractCmd p cmd
      in  Right (cmd, res)
    (False, False, True) ->
      let cmd = SMove (LongLinDiff Z $ fromIntegral $ clamp15 dz)
          res = subtractCmd p cmd
      in  Right (cmd, res)
    _ -> Left p

-- | NOTE: This does not check if all intermediate voxels are free!
-- We will need more clever algorithm.
moveCommands :: P3d -> [Command]
moveCommands (0,0,0) = []
moveCommands p =
  case extractMove p of
    Left p'@(dx, dy, dz) ->
      if dx == 0 && dy == 0 && dz == 0
        then []
        else let (axis,md) = minimumBy (comparing snd) $ zip [0..] [abs dx, abs dy, abs dz]
                 (diff1, diff2) = case axis of
                                   0 -> ((dx, 0, 0), (0, dy, dz))
                                   1 -> ((0, dy, 0), (dx, 0, dz))
                                   2 -> ((0, 0, dz), (dx, dy, 0))
             in  moveCommands diff1 ++ moveCommands diff2
    Right (cmd, p') -> cmd : moveCommands p'

-- | Move one bot in series of steps
move :: BID -> P3 -> Generator ()
move bid newPos@(nx, ny, nz) = do
  bot <- getBot bid
  let pos@(x,y,z) = _pos bot
      diff = (fromIntegral nx - fromIntegral x, fromIntegral ny - fromIntegral y, fromIntegral nz - fromIntegral z)
      commands = moveCommands diff
  forM_ commands $ \cmd -> do
      issue bid cmd
      step
  let bot' = bot {_pos = newPos}
  modify $ \st -> st {gsBots = gsBots st // [(bid, bot')]}

isFreeInModel :: P3 -> Generator Bool
isFreeInModel p = do
  matrix <- gets (mfMatrix . gsModel)
  return $ not $ matrix BA.! p

isFilledInModel :: P3 -> Generator Bool
isFilledInModel p = do
  matrix <- gets (mfMatrix . gsModel)
  return $ matrix BA.! p

-- | Returns True if voxel was not filled by generator yet
isFree :: P3 -> Generator Bool
isFree p = do
  matrix <- gets gsFilled
  return $ not $ matrix BA.! p

-- | Returns True if voxel is already filled by the generator
isFilled :: P3 -> Generator Bool
isFilled p = do
  matrix <- gets gsFilled
  return $ matrix BA.! p

makeTrace :: ModelFile -> Generator a -> IO [Command]
makeTrace model gen = do
  st <- execStateT gen =<< initState model
  let traces = gsTraces st
      maxLen = maximum $ map length $ elems traces
      botsAliveAtStep step = last [bots | (s, bots) <- gsAliveBots st, s <= step]
      botsCommandsAtStep step = map (botCommand step) [traces ! bid | bid <- botsAliveAtStep step]
      botCommand step trace =
        if step < length trace
          then trace `Seq.index` step
          else Wait
  return $ concatMap botsCommandsAtStep [0 .. maxLen-1]

test1 :: Generator ()
test1 = do
  [bid] <- getBids
  issue bid Flip
  step
  issue bid $ Fill (NearDiff 0 1 0)
  step
  move bid (5, 0, 5)
  issue bid Halt
  step

