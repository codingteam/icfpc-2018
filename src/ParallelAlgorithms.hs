module ParallelAlgorithms where

import Control.Monad.State
import qualified Data.Array as Array
import Data.Word

import Generator
import Model
import Sim

getCurrentlyAliveBots :: Generator [BotState]
getCurrentlyAliveBots = do
  aliveBots <- gets gsAliveBots
  let aliveBotBids = snd $ head aliveBots
  allBots <- gets gsBots
  return $ map (allBots Array.!) aliveBotBids

raiseAll :: Word8 -> Generator ()
raiseAll y = do
  aliveBots <- getCurrentlyAliveBots
  forM_ aliveBots $ \bot -> do
    let (x, _, z) = _pos bot
    move (_bid bot) (x, y, z)

parallelFillLayer :: Int -> Generator ()
parallelFillLayer layerNum = do
  raiseAll $ fromIntegral $ layerNum + 1
  -- TODO: adjustBotCount (layerSize)
  -- TODO: parallelFillLines

parallelFill :: Generator ()
parallelFill = do
  r <- gets (mfResolution . gsModel)
  forM_ [0 .. r-1] $ \y -> do
    parallelFillLayer $ fromIntegral y
  -- TODO: mergeAll
  -- TODO: home
