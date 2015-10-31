{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Scrabble where

import Data.Char

newtype Score = Score Int
  deriving (Eq, Ord, Num, Real, Enum, Integral, Show, Read)

instance Monoid Score where
  mempty = Score 0
  mappend = (+)

score' ::  Char -> Score
score' 'A' = 1
score' 'B' = 3
score' 'C' = 3
score' 'D' = 2
score' 'E' = 1
score' 'F' = 4
score' 'G' = 2
score' 'H' = 4
score' 'I' = 1
score' 'J' = 8
score' 'K' = 5
score' 'L' = 1
score' 'M' = 3
score' 'N' = 1
score' 'O' = 1
score' 'P' = 3
score' 'Q' = 10
score' 'R' = 1
score' 'S' = 1
score' 'T' = 1
score' 'U' = 1
score' 'V' = 4
score' 'W' = 4
score' 'X' = 8
score' 'Y' = 4
score' 'Z' = 10
score' _ = 0

score :: Char -> Score
score = score' . toUpper

scoreString :: String -> Score
scoreString = sum . map score
