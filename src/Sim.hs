{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}

module Sim (
  BID, Seed, P3,
  BotState (..),
  WorldState,
  Harmonics(..),
  Voxel(..),
  Resolution,
  Matrix,
  isWellFormedState,
  activeBots,
  isGrounded,
  enumVoxels,
  evalSingleBotCmd
) where

import Control.Monad.Except
import Control.Monad.State
import Control.Lens
import Data.List (intersect, union, nub) -- yes the horrible O(n²)
import Data.Word

-- TODO: write SimFast.hs using efficient representations

import Trace (Command, CommandF(..))

type BID = Word8
type Seed = Word8
type P3 = (Word8, Word8, Word8)

data BotState = Bot {
  _bid :: BID,
  _pos :: P3,
  _seeds :: [Seed]
} deriving (Show, Eq)
makeLenses ''BotState

data Harmonics = Low | High deriving (Show, Eq)

type Resolution = Word8

data Voxel = Void | Full deriving (Show, Eq)

newtype Matrix = Matrix (Resolution, P3 -> Voxel)

data WorldState = NMMS {
  _energy :: Integer,
  _harmonics :: Harmonics,
  _matrix :: Matrix,
  _bots :: [BotState],
  _trace :: [Command]
}
makeLenses ''WorldState

-- TODO: lenses could really nicely replace RecordWildCards

isWellFormedState :: WorldState -> Bool
isWellFormedState NMMS{..} = and [
  groundedInLowState,
  botsHaveDistinctIds,
  botsPositionedCorrectly,
  botsHaveDisjointSeeds,
  noAliveSeeds
  ] where

  Matrix (res, matF) = _matrix

  botsHaveDistinctIds
    = length _bots == length (nub $ _bid <$> _bots)

  botsPositionedCorrectly
    =  all ((==Void) . matF . _pos) _bots
    && length _bots == length (nub $ _pos <$> _bots)

  botsHaveDisjointSeeds = let seedsets = _seeds <$> _bots
    in length (nub $ concat seedsets) == sum (map length seedsets)

  noAliveSeeds = null $ map _bid _bots `intersect` concatMap _seeds _bots

  groundedInLowState = _harmonics == Low && isGrounded _matrix


lookupBot :: WorldState -> BID -> Maybe BotState
lookupBot w b = lookup b (zip (_bid <$> bots) bots)
  where bots = _bots w

activeBots :: WorldState -> [BID]
activeBots w = _bid <$> _bots w

enumVoxels :: Matrix -> [P3]
enumVoxels (Matrix (res, matF))
  = [
    (x, y, z) | x <- [0..res-1], y <- [0..res-1], z <- [0..res-1],
    matF (x, y, z) == Full
  ]

-- instance Traversable Matrix

isGrounded :: Matrix -> Bool
isGrounded = undefined

--------------------------------------------------------------------------------
type SimErr = String
type SimMonadStack1 = StateT WorldState (Except SimErr)

class (MonadError SimErr m, MonadState WorldState m) => SimMonad m where

runSimStack1 :: SimMonadStack1 a -> WorldState -> Either SimErr (a, WorldState)
runSimStack1 action = runExcept . runStateT action

type BotCmd = CommandF BID

evalSingleBotCmd :: SimMonad m => BotCmd -> m ()
evalSingleBotCmd (Wait _) = return ()

evalSingleBotCmd (Flip _) = get >>= \world@NMMS{..} -> do
  when (_harmonics == High && not (isGrounded _matrix)) $
    throwError "attempt to Flip from High non-grounded state"
  modify $ over harmonics flip
  where
    flip High = Low
    flip Low = High

evalSingleBotCmd (SMove lld bid) = undefined
--------------------------------------------------------------------------------
