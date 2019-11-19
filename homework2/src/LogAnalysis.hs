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

-- Exercise 3
build :: [LogMessage] -> MessageTree
build = foldr insert Leaf

-- Exercise 4
inOrder :: MessageTree -> [LogMessage]
inOrder Leaf = []
inOrder (Node left message right) = inOrder left ++ [message] ++ inOrder right

-- Exercise 5
whatWentWrong :: [LogMessage] -> [String]
whatWentWrong = map (\(LogMessage _ _ msg) -> msg) . filter isWrong . inOrder . build
  where isWrong (LogMessage (Error severity) _ _)
          | severity >= 50 = True
          | otherwise      = False
        isWrong _ = False
