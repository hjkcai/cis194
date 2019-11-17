module Main where

import Log
import LogAnalysis

printParse :: (String -> [LogMessage])
           -> Int
           -> FilePath
           -> IO ()
printParse parse n file = do
  logs <- testParse parse n file
  mapM_ print logs

main :: IO ()
main = do
  printParse parse 10 "error.log"
