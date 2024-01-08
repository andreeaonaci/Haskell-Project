module Main where

import App
import qualified System.Environment  as ENV

main :: IO ()
main = do
  argList <- ENV.getArgs
  progName <- ENV.getProgName
  main' progName argList
