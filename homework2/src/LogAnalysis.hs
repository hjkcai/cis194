{-# OPTIONS_GHC -Wall #-}
module LogAnalysis where

import Log

-- Exercise 1
parseMessage :: String -> LogMessage
parseMessage msg = parseMessageWords $ words msg
  where parseMessageWords ("I":timestamp:xs) = LogMessage Info (read timestamp) (unwords xs)
        parseMessageWords ("W":timestamp:xs) = LogMessage Info (read timestamp) (unwords xs)
        parseMessageWords ("E":code:timestamp:xs) = LogMessage (Error (read code)) (read timestamp) (unwords xs)
        parseMessageWords xs = Unknown $ unwords xs

parse :: String -> [LogMessage]
parse = map parseMessage . lines

-- Exercise 2
insert :: LogMessage -> MessageTree -> MessageTree
insert (Unknown _) tree = tree
insert message Leaf     = Node Leaf message Leaf
insert message@(LogMessage _ timestamp _) (Node left nodeMessage@(LogMessage _ nodeTimestamp _) right)
  | timestamp < nodeTimestamp = Node (insert message left) nodeMessage right
  | otherwise                 = Node left nodeMessage (insert message right)
insert _ _ = undefined
