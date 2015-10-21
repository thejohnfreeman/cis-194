{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module LogAnalysis where

import Data.List (foldl')
import Test.HUnit
import Log

parseMessage :: String -> LogMessage
parseMessage line = case words line of
  (ty:level:time:rest)
    | ty == "E" -> LogMessage (Error $ read level) (read time) (unwords rest)
  (ty:time:rest)
    | ty == "I" -> LogMessage Info (read time) (unwords rest)
    | ty == "W" -> LogMessage Warning (read time) (unwords rest)
  _ -> Unknown line

parse :: String -> [LogMessage]
parse = map parseMessage . lines

instance Ord LogMessage where
  compare (LogMessage _ t1 _) (LogMessage _ t2 _) = compare t1 t2
  compare _ _ = error "cannot compare Unknown"

insert :: LogMessage -> MessageTree -> MessageTree
insert (Unknown _) t = t
insert m Leaf = Node Leaf m Leaf
insert m (Node l m' r)
  | m < m'    = Node (insert m l) m' r
  | otherwise = Node l m' (insert m r)

build :: [LogMessage] -> MessageTree
build = foldl' (flip insert) Leaf

inOrder :: MessageTree -> [LogMessage]
inOrder Leaf = []
inOrder (Node l m r) = inOrder l ++ [m] ++ inOrder r

whatWentWrong :: [LogMessage] -> [String]
whatWentWrong = map getMsg . filter isSevere . inOrder . build
  where
    isSevere (LogMessage (Error l) _ _) | l >= 50 = True
    isSevere _ = False
    getMsg (LogMessage _ _ msg) = msg
    getMsg _ = error "unreachable"

main :: IO ()
main = do
  _ <- runTestTT $ test
    [ "parse1" ~: parseMessage "E 2 562 help help" ~?= LogMessage (Error 2) 562 "help help"
    , "parse2" ~: parseMessage "I 29 la la la" ~?= LogMessage Info 29 "la la la"
    , "parse3" ~: parseMessage "This is not in the right format" ~?= Unknown "This is not in the right format"
    --, "parse4" ~: length (testParse parse 10 "error.log") ~?= 10
    ]
  return ()

