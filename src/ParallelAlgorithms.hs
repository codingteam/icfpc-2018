module ParallelAlgorithms where

import Control.Monad.State
import qualified Data.Array as Array
import qualified Data.Array.BitArray as BitArray
import qualified Data.List as List
import Data.Ord
import Data.Word

import Generator
import Model
import Sim
import Trace

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

getLayerLines :: Word8 -> Generator [Word8]
getLayerLines layerNum = do
  model <- gets gsModel
  let matrix = mfMatrix model
  let resolution = mfResolution model
  let needVoxels lineNum = any (\x -> matrix BitArray.! (x, layerNum, lineNum)) [0 .. resolution - 1]
  return $ filter needVoxels [0 .. resolution - 1]

getPreferredChildCount :: Word8 -> Word8
getPreferredChildCount lineCount =
  min maxBID lineCount

isRootBot :: BotState -> Bool
isRootBot b = _bid b == 0

rootBotAdjustAction :: [Word8] -> Generator ()
rootBotAdjustAction linesToFill = do
  let childCount = getPreferredChildCount $ fromIntegral $ length linesToFill
  aliveBots <- getCurrentlyAliveBots
  let aliveBotCount = fromIntegral $ length aliveBots
  let myself = head $ filter isRootBot aliveBots
  let isNextLineFree =
        let coord = nearPlus (_pos myself) $ NearDiff 0 0 1
        in not $ any (\b -> _pos b == coord) aliveBots
  let forkNextLine = do
        issueFission (_bid myself) 0 (NearDiff 0 0 1)
        return ()
  let wait = do
        issue (_bid myself) Wait
  if childCount > aliveBotCount && isNextLineFree
    then forkNextLine
    else wait

getAssignedLines :: [BotState] -> [Word8] -> [(BotState, Word8)]
getAssignedLines bots lines =
  let childCount = getPreferredChildCount $ fromIntegral $ length lines
      lastLines = drop (length lines - fromIntegral childCount) lines
      children = filter (not . isRootBot) children
      sortedChildren = List.sortBy (comparing getMyLine) children
  in zip sortedChildren lastLines
  where getMyLine bot = 
          let (_, _, z) = _pos bot
          in z

startOfLine :: ModelFile -> Word8 -> Word8 -> P3
startOfLine model layer line =
  let resolution = mfResolution model
      matrix = mfMatrix model
      coords = map (\x -> (x, layer, line)) [0..resolution - 1]
  in head $ filter (\p -> getVoxel matrix p == Full) coords

childBotAdjustActions :: Word8 -> [Word8] -> Generator ()
childBotAdjustActions layer lines = do
  aliveBots <- getCurrentlyAliveBots
  let assignedLines = getAssignedLines aliveBots lines
  let childBotAssignments = filter (not . isRootBot . fst) assignedLines
  model <- gets gsModel
  let moveToLine bot lineNum = do
        move (_bid bot) $ startOfLine model layer lineNum
  let wait bot = do
        issue (_bid bot) Wait
  forM_ childBotAssignments $ \(bot, lineNum) -> do
    let inPlace = getLineNum bot == lineNum
    if inPlace
      then wait bot
      else moveToLine bot lineNum
    where
      getLineNum bot =
        let (_, _, z) = _pos bot
        in z

allInPosition :: Word8 -> [Word8] -> Generator Bool
allInPosition layer lines = do
  aliveBots <- getCurrentlyAliveBots
  let assignedLines = getAssignedLines aliveBots lines
  model <- gets gsModel
  return $ all (\(bot, lineNum) -> _pos bot == startOfLine model layer lineNum) assignedLines

adjustBots :: Word8 -> [Word8] -> Generator ()
adjustBots layer lines = do
  rootBotAdjustAction lines
  childBotAdjustActions layer lines
  proceed <- allInPosition layer lines
  if not proceed
    then adjustBots layer lines
    else return ()

parallelFillLayer :: Word8 -> Generator ()
parallelFillLayer layerNum = do
  raiseAll $ layerNum + 1
  layerLines <- getLayerLines layerNum
  adjustBots layerNum layerLines
  -- TODO: parallelFillLines

parallelFill :: Generator ()
parallelFill = do
  r <- gets (mfResolution . gsModel)
  forM_ [0 .. r-2] $ \y -> do
    parallelFillLayer y
  -- TODO: mergeAll
  -- TODO: home
