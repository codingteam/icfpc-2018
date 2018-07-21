
module Generator where

import Control.Monad
import Control.Monad.State
import Data.Maybe
import Data.Array
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

data GeneratorState = GS {
    gsModel :: ModelFile,
    gsFilled :: BA.BitArray P3,
    gsStepNumber :: Step,
    gsAliveBots :: [(Step, [AliveBot])], -- which bots are alive. Record is to be added when set of bots is changed.
    gsBots :: Array BID BotState,
    gsTraces :: Array BID BotTrace -- Trace is to be filled with Wait if bot does nothing or is not alive.
  }

maxBID :: BID
maxBID = 20

initState :: ModelFile -> GeneratorState
initState model = GS model filled 0 [(0,[bid])] bots traces
  where
    bid = 0
    bots   = array (0, maxBID) [(bid, Bot bid (0,0,0) []) | bid <- [0 .. maxBID]]
    traces = array (0, maxBID) [(bid, []) | bid <- [0 .. maxBID]]
    r = mfResolution model
    filled = BA.array ((0,0,0), (r-1,r-1,r-1)) [((x,y,z), False) | x <- [0..r-1], y <- [0..r-1], z <- [0..r-1]]

type Generator a = State GeneratorState a

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

type I3 = (Int,Int,Int)

substractCmd :: I3 -> Command -> I3
substractCmd (dx, dy, dz) (SMove (LongLinDiff X dx1)) = (dx-dx1, dy, dz)
substractCmd (dx, dy, dz) (SMove (LongLinDiff Y dy1)) = (dx, dy-dy1, dz)
substractCmd (dx, dy, dz) (SMove (LongLinDiff Z dz1)) = (dx, dy, dz-dz1)
substractCmd (dx, dy, dz) (LMove (ShortLinDiff X dx1) (ShortLinDiff X dx2)) = (dx-dx1-dx2, dy, dz)
substractCmd (dx, dy, dz) (LMove (ShortLinDiff Y dy1) (ShortLinDiff Y dy2)) = (dx, dy-dy1-dy2, dz)
substractCmd (dx, dy, dz) (LMove (ShortLinDiff Z dz1) (ShortLinDiff Z dz2)) = (dx, dy, dz-dz1-dz2)
substractCmd (dx, dy, dz) (LMove (ShortLinDiff X dx1) (ShortLinDiff Y dy2)) = (dx-dx1, dy-dy2, dz)
substractCmd (dx, dy, dz) (LMove (ShortLinDiff X dx1) (ShortLinDiff Z dz2)) = (dx-dx1, dy, dz-dz2)
substractCmd (dx, dy, dz) (LMove (ShortLinDiff Y dy1) (ShortLinDiff Z dz2)) = (dx, dy-dy1, dz-dz2)
substractCmd p (LMove sld1 sld2) = substractCmd p (LMove sld2 sld1)
substractCmd _ c = error $ "Impossible move command: " ++ show c

origin :: P3
origin = (0,0,0)

extractMove :: I3 -> Either I3 (Command, I3)
extractMove p@(dx, dy, dz) =
  case (dx /= 0, dy /= 0, dz /= 0) of
    (True, True, False) ->
      let cmd = LMove (ShortLinDiff X (min dx 5)) (ShortLinDiff Y (min dy 5))
          res = substractCmd p cmd
      in  Right (cmd, res)
    (True, False, True) ->
      let cmd = LMove (ShortLinDiff X (min dx 5)) (ShortLinDiff Z (min dz 5))
          res = substractCmd p cmd
      in  Right (cmd, res)
    (False, True, True) ->
      let cmd = LMove (ShortLinDiff Y (min dy 5)) (ShortLinDiff Z (min dz 5))
          res = substractCmd p cmd
      in  Right (cmd, res)
    (True, False, False) ->
      let cmd = SMove (LongLinDiff X $ min dx 15)
          res = substractCmd p cmd
      in  Right (cmd, res)
    (False, True, False) ->
      let cmd = SMove (LongLinDiff Y $ min dy 15)
          res = substractCmd p cmd
      in  Right (cmd, res)
    (False, False, True) ->
      let cmd = SMove (LongLinDiff Z $ min dz 15)
          res = substractCmd p cmd
      in  Right (cmd, res)
    _ -> Left p
    
-- | NOTE: This does not check if all intermediate voxels are free!
-- We will need more clever algorithm.
moveCommands :: I3 -> [Command]
moveCommands (0,0,0) = []
moveCommands p =
  case extractMove p of
    Left p' -> if p' == (0,0,0)
                 then []
                 else error $ "Cannot do such move: " ++ show p'
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

