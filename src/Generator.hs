
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

-- | Issue one command for one bot.
issue :: BID -> Command -> Generator ()
issue bid cmd = do
  checkBid bid
  trace <- getBotTrace bid
  let trace' = trace ++ [cmd]
  modify $ \st -> st {gsTraces = gsTraces st // [(bid, trace')]}

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

-- move :: P3 -> Generator ()

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
  issue bid $ Fill (NearDiff 1 1 1)
  step
  issue bid Halt
  step

