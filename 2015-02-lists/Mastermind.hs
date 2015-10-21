{-# OPTIONS_GHC -Wall #-}
module Mastermind where

import Test.HUnit

-- Mastermind -----------------------------------------

-- A peg can be one of six colors
data Peg = Red | Green | Blue | Yellow | Orange | Purple
         deriving (Show, Eq, Ord)

-- A code is defined to simply be a list of Pegs
type Code = [Peg]

-- A move is constructed using a Code and two integers; the number of
-- exact matches and the number of regular matches
data Move = Move Code Int Int
          deriving (Show, Eq)

-- List containing all of the different Pegs
colors :: [Peg]
colors = [Red, Green, Blue, Yellow, Orange, Purple]

-- Exercise 1 -----------------------------------------

-- Get the number of exact matches between the actual code and the guess
exactMatches :: Code -> Code -> Int
exactMatches as bs = length $ filter id $ zipWith (==) as bs

-- Exercise 2 -----------------------------------------

-- For each peg in xs, count how many times is occurs in ys
countColors :: Code -> [Int]
countColors cs = map countColor colors
  where
    countColor c = length $ filter (c ==) cs

-- Count number of matches between the actual code and the guess
matches :: Code -> Code -> Int
matches = undefined

-- Exercise 3 -----------------------------------------

-- Construct a Move from a guess given the actual code
getMove :: Code -> Code -> Move
getMove = undefined

-- Exercise 4 -----------------------------------------

isConsistent :: Move -> Code -> Bool
isConsistent = undefined

-- Exercise 5 -----------------------------------------

filterCodes :: Move -> [Code] -> [Code]
filterCodes = undefined

-- Exercise 6 -----------------------------------------

allCodes :: Int -> [Code]
allCodes = undefined

-- Exercise 7 -----------------------------------------

solve :: Code -> [Move]
solve = undefined

-- Bonus ----------------------------------------------

fiveGuess :: Code -> [Move]
fiveGuess = undefined

main :: IO ()
main = do
  _ <- runTestTT $ test
    [ "no-match" ~: exactMatches [Red, Blue, Green, Yellow] [Blue, Green, Yellow, Red] ~?= 0
    , "match" ~: exactMatches [Red, Blue, Green, Yellow] [Red, Purple, Green, Orange] ~?= 2
    , "count1" ~: countColors [Red, Blue, Yellow, Purple] ~?= [1, 0, 1, 1, 0, 1]
    , "count2" ~: countColors [Green, Blue, Green, Orange] ~?= [0, 2, 1, 0, 1, 0]
    ]
  return ()

