module ParallelAlgorithms where

import Control.Monad.State
import qualified Data.Array as Array
import qualified Data.Array.BitArray as BitArray
import qualified Data.List as List
import Data.Ord
import Data.Word
import System.IO.Unsafe

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
  -- TODO: This should be simultaneous for all bots
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

getPreferredTotalBotCount :: Word8 -> Word8
getPreferredTotalBotCount lineCount =
  min 2 lineCount -- TODO: Replace 2 with maxBID

isRootBot :: BotState -> Bool
isRootBot b = _bid b == 0

rootBotAdjustAction :: Word8 -> [Word8] -> Generator ()
rootBotAdjustAction layer linesToFill = do
  let preferredBotCount = getPreferredTotalBotCount $ fromIntegral $ length linesToFill
  aliveBots <- getCurrentlyAliveBots
  model <- gets gsModel
  let aliveBotCount = fromIntegral $ length aliveBots
  let myself = head $ filter isRootBot aliveBots
  let isNextLineFree =
        let coord = nearPlus (_pos myself) $ NearDiff 0 0 1
        in not $ any (\b -> _pos b == coord) aliveBots
  let forkNextLine = do
        let nd = (NearDiff 0 0 1)
        unsafePerformIO $ do
          putStrLn $ "Root bot decided to fork itself to: " ++ (show $ nearPlus (_pos myself) nd)
          let myBid = _bid myself
          return $ do
            issueFission myBid 0 nd
            return ()
  unsafePerformIO $ do
    putStrLn $ "Root bot detected itself: " ++ show myself
    putStrLn $ "Preferred bot count: " ++ show preferredBotCount
    putStrLn $ "Alive bot count: " ++ show aliveBotCount
    let rootBotPreferredPlace = aboveStartOfLine model layer $ head linesToFill
    putStrLn $ "Required position for root bot: " ++ show rootBotPreferredPlace
    if preferredBotCount > aliveBotCount && isNextLineFree
      then return forkNextLine
      else if _pos myself /= rootBotPreferredPlace
        then do
          putStrLn $ "Root bot decided to move to " ++ show rootBotPreferredPlace
          return $ do move (_bid myself) rootBotPreferredPlace
        else do
          putStrLn "Root bot decided to do nothing"
          return $ do return ()

getAssignedLines :: [BotState] -> [Word8] -> [(BotState, Word8)]
getAssignedLines bots lines =
  let rootBot = head $ filter isRootBot bots
      botCount = getPreferredTotalBotCount $ fromIntegral $ length lines
      lastLines = drop (length lines - fromIntegral botCount) lines
      children = filter (not . isRootBot) bots
      sortedChildren = List.sortBy (comparing getMyLine) children
  in (rootBot, head lines) : zip sortedChildren lastLines
  where getMyLine bot = 
          let (_, _, z) = _pos bot
          in z

aboveStartOfLine :: ModelFile -> Word8 -> Word8 -> P3
aboveStartOfLine model layer line =
  let resolution = mfResolution model
      matrix = mfMatrix model
      coords = map (\x -> (x, layer, line)) [0..resolution - 1]
      startOfLine = head $ filter (\p -> getVoxel matrix p == Full) coords
      (x, y, z) = startOfLine
  in (x, y + 1, z)

childBotAdjustActions :: Word8 -> [Word8] -> Generator ()
childBotAdjustActions layer lines = do
  aliveBots <- getCurrentlyAliveBots
  let assignedLines = getAssignedLines aliveBots lines
  let childBotAssignments = filter (not . isRootBot . fst) assignedLines
  model <- gets gsModel
  let moveToLine bot lineNum =
        let target = aboveStartOfLine model layer lineNum
        in unsafePerformIO $ do
            putStrLn $ "Child bot decided to move itself to " ++ show target
            return $ do move (_bid bot) target
  forM_ childBotAssignments $ \(bot, lineNum) -> do
    let inPlace = _pos bot == aboveStartOfLine model layer lineNum
    unsafePerformIO $ do
      putStrLn $ "Preparing child adjust action for " ++ show bot
      putStrLn $ "Is bot in place? " ++ show inPlace
      return $ do
        when (not inPlace) $ moveToLine bot lineNum
        where getLineNum bot =
                let (_, _, z) = _pos bot
                in z

allInPosition :: Word8 -> [Word8] -> Generator Bool
allInPosition layer lines = do
  aliveBots <- getCurrentlyAliveBots
  let assignedLines = getAssignedLines aliveBots lines
  model <- gets gsModel
  let result = all (\(bot, lineNum) -> _pos bot == aboveStartOfLine model layer lineNum) assignedLines
  unsafePerformIO $ do
    putStrLn "Checking whether everyone is in position..."
    putStrLn $ "Assigned positions for everyone: " ++ (show $ map (\(_, lineNum) -> aboveStartOfLine model layer lineNum) assignedLines)
    putStrLn $ "Actual positions: " ++ show aliveBots
    putStrLn $ "Is everyone in place? " ++ show result
    return $ do
      return result

adjustBots :: Word8 -> [Word8] -> Generator ()
adjustBots layer lines = do
  when (not $ null lines) $ do
    currentStep <- gets gsStepNumber
    unsafePerformIO $ do
      putStrLn $ "Adjusting bots, step " ++ show currentStep ++ ", layer " ++ show layer
      return $ do
        childBotAdjustActions layer lines
        rootBotAdjustAction layer lines
        step
        proceed <- allInPosition layer lines
        if not proceed
          then adjustBots layer lines
          else unsafePerformIO $ do
            putStrLn $ "Successfully adjusted for layer " ++ show layer
            return $ do return ()

parallelFillLayer :: Word8 -> Generator ()
parallelFillLayer layerNum = do
  raiseAll $ layerNum + 1
  layerLines <- getLayerLines layerNum
  adjustBots layerNum layerLines
  -- TODO: parallelFillLines
  -- TODO: fuseRedundantBots

parallelFill :: Generator ()
parallelFill = do
  r <- gets (mfResolution . gsModel)
  forM_ [0 .. r-2] $ \y -> do
    parallelFillLayer y
  -- TODO: mergeAll
  -- TODO: home
