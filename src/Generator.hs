
module Generator where

import Control.Monad
import Control.Monad.State
import Data.Maybe
import Data.Array

import Trace
import Sim

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

-- TODO: put Model here.
data GeneratorState = GS {
    gsStepNumber :: Step,
    gsAliveBots :: [(Step, [AliveBot])], -- which bots are alive. Record is to be added when set of bots is changed.
    gsBots :: [BotState],
    gsTraces :: Array BID BotTrace -- Trace is to be filled with Wait if bot does nothing or is not alive.
  }
  deriving (Show)

maxBID :: BID
maxBID = 20

initState :: GeneratorState
initState = GS 0 [(0,[bid])] [bot] traces
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

substractCmd :: P3 -> Command -> P3
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

extractMove :: P3 -> Either P3 (Command, P3)
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
moveCommands :: P3 -> [Command]
moveCommands (0,0,0) = []
moveCommands p =
  case extractMove p of
    Left p' -> if p' == origin
                 then []
                 else error $ "Cannot do such move: " ++ show p'
    Right (cmd, p') -> cmd : moveCommands p'

-- | Move one bot in series of steps
move :: BID -> P3 -> Generator ()
move bid newPos@(nx, ny, nz) = do  
  bot <- getBot bid
  let pos@(x,y,z) = _pos bot
      diff = (nx-x, ny-y, nz-z)
      commands = moveCommands diff
  forM_ commands $ \cmd -> do
      issue bid cmd
      step

makeTrace :: Generator a -> [Command]
makeTrace gen =
  let st = execState gen initState
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

