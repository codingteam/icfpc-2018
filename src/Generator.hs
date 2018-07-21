
module Generator where

import Control.Monad
import Control.Monad.State
import Data.Maybe
import Data.Array
import Data.Int
import qualified Data.Array.BitArray as BA

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

type BotTrace = [Command]

type Step = Int

type AliveBot = BID

-- | Offset relative to some point in the matrix.
type P3d = (Int16, Int16, Int16)

data GeneratorState = GS {
    gsModel :: ModelFile,
    gsStepNumber :: Step,
    gsAliveBots :: [(Step, [AliveBot])], -- which bots are alive. Record is to be added when set of bots is changed.
    gsBots :: [BotState],
    gsTraces :: Array BID BotTrace -- Trace is to be filled with Wait if bot does nothing or is not alive.
  }
  deriving (Show)

maxBID :: BID
maxBID = 20

initState :: ModelFile -> GeneratorState
initState model = GS model 0 [(0,[bid])] [bot] traces
  where
    bid = 0
    bot = Bot bid (0,0,0) []
    traces = array (0, maxBID) [(bid, []) | bid <- [0 .. maxBID]]

type Generator a = State GeneratorState a

getBotTrace :: BID -> Generator BotTrace
getBotTrace bid = do
  traces <- gets gsTraces
  return $ traces ! bid

-- Check that there is such bot
checkBid :: BID -> Generator ()
checkBid bid = do
  bots <- gets gsBots
  when (bid `notElem` map _bid bots) $
      fail $ "There is currently no such bot: " ++ show bid ++ "\nCurrent bots are:\n" ++ show bots

getBids :: Generator [BID]
getBids = do
  bots <- gets gsBots
  return $ map _bid bots

getBot :: BID -> Generator BotState
getBot bid = do
  bots <- gets gsBots
  let good = [bot | bot <- bots, _bid bot == bid]
  if null good
    then fail $ "No such bot: " ++ show bid
    else return $ head good

-- | Issue one command for one bot.
issue :: BID -> Command -> Generator ()
issue bid cmd = do
  checkBid bid
  trace <- getBotTrace bid
  let trace' = trace ++ [cmd]
  modify $ \st -> st {gsTraces = gsTraces st // [(bid, trace')]}

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
                           then Just (bid, trace ++ [Wait])
                           else Nothing
    let traces' = traces // updates
    modify $ \st -> st {gsStepNumber = n', gsTraces = traces'}

substractCmd :: P3d -> Command -> P3d
substractCmd (dx, dy, dz) (SMove (LongLinDiff X dx1)) = (dx-(fromIntegral dx1), dy, dz)
substractCmd (dx, dy, dz) (SMove (LongLinDiff Y dy1)) = (dx, dy-(fromIntegral dy1), dz)
substractCmd (dx, dy, dz) (SMove (LongLinDiff Z dz1)) = (dx, dy, dz-(fromIntegral dz1))
substractCmd (dx, dy, dz) (LMove (ShortLinDiff X dx1) (ShortLinDiff X dx2)) = (dx-(fromIntegral dx1)-(fromIntegral dx2), dy, dz)
substractCmd (dx, dy, dz) (LMove (ShortLinDiff Y dy1) (ShortLinDiff Y dy2)) = (dx, dy-(fromIntegral dy1)-(fromIntegral dy2), dz)
substractCmd (dx, dy, dz) (LMove (ShortLinDiff Z dz1) (ShortLinDiff Z dz2)) = (dx, dy, dz-(fromIntegral dz1)-(fromIntegral dz2))
substractCmd (dx, dy, dz) (LMove (ShortLinDiff X dx1) (ShortLinDiff Y dy2)) = (dx-(fromIntegral dx1), dy-(fromIntegral dy2), dz)
substractCmd (dx, dy, dz) (LMove (ShortLinDiff X dx1) (ShortLinDiff Z dz2)) = (dx-(fromIntegral dx1), dy, dz-(fromIntegral dz2))
substractCmd (dx, dy, dz) (LMove (ShortLinDiff Y dy1) (ShortLinDiff Z dz2)) = (dx, dy-(fromIntegral dy1), dz-(fromIntegral dz2))
substractCmd p (LMove sld1 sld2) = substractCmd p (LMove sld2 sld1)
substractCmd _ c = error $ "Impossible move command: " ++ show c

origin :: P3
origin = (0,0,0)

clamp :: Ord a => (a,  a) -> a -> a
clamp (low, high) value
  | value >= low && value <= high = max low (min value high)
  | otherwise = error "clamp: value is outside of clamping range"

extractMove :: P3d -> Either P3d (Command, P3d)
extractMove p@(dx, dy, dz) =
  let clamp5 = clamp (-5, 5)
  in case (dx /= 0, dy /= 0, dz /= 0) of
    (True, True, False) ->
      let cmd = LMove (ShortLinDiff X (fromIntegral $ clamp5 dx)) (ShortLinDiff Y (fromIntegral $ clamp5 dy))
          res = substractCmd p cmd
      in  Right (cmd, res)
    (True, False, True) ->
      let cmd = LMove (ShortLinDiff X (fromIntegral $ clamp5 dx)) (ShortLinDiff Z (fromIntegral $ clamp5 dz))
          res = substractCmd p cmd
      in  Right (cmd, res)
    (False, True, True) ->
      let cmd = LMove (ShortLinDiff Y (fromIntegral $ clamp5 dy)) (ShortLinDiff Z (fromIntegral $ clamp5 dz))
          res = substractCmd p cmd
      in  Right (cmd, res)
    (True, False, False) ->
      let cmd = SMove (LongLinDiff X $ fromIntegral $ clamp5 dx)
          res = substractCmd p cmd
      in  Right (cmd, res)
    (False, True, False) ->
      let cmd = SMove (LongLinDiff Y $ fromIntegral $ clamp5 dy)
          res = substractCmd p cmd
      in  Right (cmd, res)
    (False, False, True) ->
      let cmd = SMove (LongLinDiff Z $ fromIntegral $ clamp5 dz)
          res = substractCmd p cmd
      in  Right (cmd, res)
    _ -> Left p

-- | NOTE: This does not check if all intermediate voxels are free!
-- We will need more clever algorithm.
moveCommands :: P3d -> [Command]
moveCommands (0,0,0) = []
moveCommands p =
  case extractMove p of
    Left p'@(x, y, z) -> if x == 0 && y == 0 && z == 0
                 then []
                 else error $ "Cannot do such move: " ++ show p'
    Right (cmd, p') -> cmd : moveCommands p'

-- | Move one bot in series of steps
move :: BID -> P3 -> Generator ()
move bid newPos@(nx, ny, nz) = do
  bot <- getBot bid
  let pos@(x,y,z) = _pos bot
      diff = ((fromIntegral nx)-(fromIntegral x), (fromIntegral ny)-(fromIntegral y), (fromIntegral nz)-(fromIntegral z))
      commands = moveCommands diff
  forM_ commands $ \cmd -> do
      issue bid cmd
      step

isFree :: P3 -> Generator Bool
isFree p = do
  matrix <- gets (mfMatrix . gsModel)
  return $ matrix BA.! p

makeTrace :: ModelFile -> Generator a -> [Command]
makeTrace model gen =
  let st = execState gen (initState model)
      traces = gsTraces st
      maxLen = maximum $ map length $ elems traces
      botsAliveAtStep step = last [bots | (s, bots) <- gsAliveBots st, s <= step]
      botsCommandsAtStep step = map (botCommand step) [traces ! bid | bid <- botsAliveAtStep step]
      botCommand step trace =
        if step < length trace
          then trace !! step
          else Wait
  in  concatMap botsCommandsAtStep [0 .. maxLen-1]

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

