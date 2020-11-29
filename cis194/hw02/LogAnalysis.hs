{-# OPTIONS_GHC -Wall #-}
module LogAnalysis where

import Log

-- readInt   :: String -> Int

joinString :: [String] -> String
joinString [] = ""
joinString [x] = x
joinString (x:xs) = x ++ " " ++ joinString xs

readInt x = read x :: Int

parseMessage :: String -> LogMessage
parseMessage msg = case msg of 
    ('E':rest) -> LogMessage (Error $ readInt $ head errorWords) (readInt $ errorWords !! 1) (joinString $ drop 2 errorWords)
        where errorWords = words rest
    ('I':rest) -> LogMessage Info (readInt $ head errorWords) (joinString $ drop 1 errorWords)
        where errorWords = words rest
    _  -> Unknown msg

parse :: String -> [LogMessage]
parse log = map parseMessage $ lines log
-- Test this function with testParse parse 20 "error.log"

insert :: LogMessage -> MessageTree -> MessageTree
-- insert msg tree = 