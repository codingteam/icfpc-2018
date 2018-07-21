module Main where

import System.Environment

import Trace
import Model

main :: IO ()
main = do
  [cmd, file] <- getArgs
  case cmd of
    "display" -> displayModelFile file
    _ -> fail "unknown command"
