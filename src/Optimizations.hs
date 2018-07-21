-- | Optimizations for sequences of commands.
--
-- Each optimizations replaces first occurrence of a pattern with an optimized
-- version.  It's up to you to figure out if the result is better than the
-- input, and if you should re-run optimizations to find new occurrences of
-- patterns.
--
-- WARNING: For now, these functions assume that there is only one nanobot!
module Optimizations where

import Control.Monad (forM)

import Trace

import Debug.Trace

-- | Returns all possible optimizations of a given command sequence.
optimize :: [Command] -> [[Command]]
optimize [] = []
optimize cmds = go [cmds] [cmds]
  where
  go :: [[Command]] -> [[Command]] -> [[Command]]
  go [] optimized = optimized
  go (t:stack) optimized =
    let
      new = map (\f -> f t) optimizers
      new' = filter (/= t) new
    in go (new' ++ stack) (new' ++ optimized)

  optimizers :: [[Command] -> [Command]]
  optimizers = [splitLMove, mergeSMoves]

-- | Turns [LMove a b, Wait] into [SMove a, SMove b]
--
-- The latter consumes 4 less energy points than the former.
--
-- Note: it doesn't make sense to split [LMove a b] into two SMoves because the
-- extra step will consume 3 R³ energy points, which is detrimental for any
-- R greater than one.
splitLMove :: [Command] -> [Command]
splitLMove [] = []
splitLMove ((LMove a b):Wait:cmds) =
  (SMove (fromShortLinDiff a)) : (SMove (fromShortLinDiff b)) : cmds
splitLMove (cmd:cmds) = cmd : (splitLMove cmds)

-- | Turns [SMove a, SMove b] into [SMove x] if a and b are on the same axis
-- and can be combined.
mergeSMoves :: [Command] -> [Command]
mergeSMoves [] = []
mergeSMoves (m1@(SMove (LongLinDiff a1 d1)):m2@(SMove (LongLinDiff a2 d2)):cmds)
  | (a1 /= a2) || (d1 + d2 < -15) || (d1 + d2 > 15)
      = m1 : (mergeSMoves (m2:cmds))
  | otherwise =
      let d = d1 + d2
      in if d == 0
        then cmds
        else (SMove (LongLinDiff a1 d)) : cmds
mergeSMoves (cmd:cmds) = cmd : (mergeSMoves cmds)
