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

printTree :: MessageTree -> IO ()
printTree Leaf = return ()
printTree (Node left message right) = do
  printTree left
  print message
  printTree right

main :: IO ()
main = do
  printParse parse 10 "error.log"
