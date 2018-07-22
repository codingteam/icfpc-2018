
module Evaluator where

import Control.Monad
import Control.Monad.State
import Data.Word
import Data.Binary (decodeFile)
import Data.List
import Data.Ord
import Text.Printf
-- import qualified Data.Array.BitArray.IO as BAIO

import Trace
import Sim
import Model

type Eval a = State EvalState a

data EvalState = EvalState {
    esResolution :: Word8,
    esTrace :: [Command],
    esActiveBots :: Word8,
    esHarmonics :: Harmonics,
    esEnergy :: Energy
  }

initState :: ModelFile -> [Command] -> EvalState
initState m trace = EvalState (mfResolution m) trace 1 Low 0

evalCmd :: Command -> Eval Energy
evalCmd Halt = return 0
evalCmd Wait = return 0
evalCmd Flip = do
  modify $ \st -> st {esHarmonics = flipH (esHarmonics st)}
  return 0
evalCmd (SMove lld) = return $ 2*mlen lld
evalCmd (LMove sld1 sld2) = return $ 2 * (mlen sld1 + 2 + mlen sld2)
evalCmd (Fission _ _) = do
  modify $ \st -> st {esActiveBots = esActiveBots st + 1}
  return 24
evalCmd (Fill _) = return 12 -- NB: we do not care if the voxel was already filled
evalCmd (DoVoid _) = return (-12)
evalCmd (FusionP _) = do
  modify $ \st -> st {esActiveBots = esActiveBots st - 1}
  return (-24)
evalCmd (FusionS _) = return 0 -- this is paired with FussionP so we do not care.
evalCmd (GFill _ _) = fail "GFill is not supported yet" -- TODO: calculate volume of the region etc
evalCmd (GVoid _ _) = fail "GVoid is not supported yet"

getCommand :: Eval (Maybe Command)
getCommand = do
  trace <- gets esTrace
  case trace of
    [] -> return Nothing
    (c:cs) -> do
      modify $ \st -> st {esTrace = cs}
      return $ Just c

addEnergy :: Energy -> Eval ()
addEnergy e =
  modify $ \st -> st {esEnergy = esEnergy st + e}

eval :: Eval ()
eval = do
    n <- gets (fromIntegral . esActiveBots)
    r <- gets (fromIntegral . esResolution)
    harmonics <- gets esHarmonics
    addEnergy $ 20 * n
    addEnergy $ case harmonics of
                  Low -> 3 * r * r * r
                  High -> 30 * r * r * r
    evalStep n
    done <- gets (null . esTrace)
    unless done $
        eval
  where
    evalStep 0 = return ()
    evalStep i = do
      mbCmd <- getCommand
      case mbCmd of
        Nothing -> return ()
        Just cmd -> do
          addEnergy =<< evalCmd cmd
          evalStep (i-1)

evalTrace :: ModelFile -> [Command] -> Energy
evalTrace m trace = esEnergy $ execState eval (initState m trace)

doEvalTrace :: FilePath -> FilePath -> IO ()
doEvalTrace modelPath tracePath = do
  model <- decodeFile modelPath
  trace <- readTrace tracePath
  let energy = evalTrace model trace
  printf "Energy: %d\n" energy

selectBest :: FilePath -> [FilePath] -> IO ()
selectBest modelPath tracePaths = do
  model <- decodeFile modelPath
  traces <- mapM readTrace tracePaths
  res <- forM (zip tracePaths traces) $ \(path, trace) -> do
           let energy = evalTrace model trace
           printf "%s : %d\n" path energy
           return (path, energy)
  let path = fst $ minimumBy (comparing snd) res
  putStrLn path

