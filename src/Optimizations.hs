-- | Optimizations for sequences of commands.
--
-- Each optimizations replaces first occurrence of a pattern with an optimized
-- version.  It's up to you to figure out if the result is better than the
-- input, and if you should re-run optimizations to find new occurrences of
-- patterns.
module Optimizations where

import Trace

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
