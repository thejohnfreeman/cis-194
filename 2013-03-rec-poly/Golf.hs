module Golf where

import Data.List hiding (lines)
import qualified Data.Map.Strict as Map
import Test.HUnit hiding (Counts)
import Prelude hiding (lines)

every :: [a] -> Int -> [a]
every xs n = [x | (i,x) <- zip [1..] xs, i `rem` n == 0]

skips :: [a] -> [[a]]
skips xs = map (every xs) [1 .. length xs]

localMaxima :: [Integer] -> [Integer]
localMaxima (x:y:z:xs)
  | x < y && y > z = y : rest
  | otherwise = rest
  where rest = localMaxima (y:z:xs)
localMaxima _ = []

histogram :: [Integer] -> String
histogram xs = intercalate "\n" $ reverse $ "0123456789" : replicate 10 '=' : lines (count xs)

type Counts k = Map.Map k Integer

count :: Ord k => [k] -> Counts k
count = foldr (Map.alter add1) Map.empty
  where
    add1 Nothing = Just 1
    add1 (Just n) = Just $ succ n

line :: Ord k => [k] -> Counts k -> Integer -> String
line keys m n = map (\k -> if n <= Map.findWithDefault 0 k m then '*' else ' ') keys
--line n map = [if n <= Map.findWithDefault 0 i map then '*' else ' ' | i <- [0..9]]

lines :: Counts Integer -> [String]
lines m = takeWhile ('*' `elem`) $ map (line [0..9] m) [1..]

main :: IO ()
main = do
  _ <- runTestTT $ test
    [ "skips1" ~: skips "ABCD" ~?= ["ABCD", "BD", "C", "D"]
    , "skips2" ~: skips "hello!" ~?= ["hello!", "el!", "l!", "l", "o", "!"]
    , "skips3" ~: skips [1 :: Integer] ~?= [[1]]
    , "skips4" ~: skips [True,False] ~?= [[True,False], [False]]
    , "skips5" ~: skips ([] :: [Bool]) ~?= []
    , "localMaxima1" ~: localMaxima [2,9,5,6,1] ~?= [9,6]
    , "localMaxima1" ~: localMaxima [2,3,4,1,5] ~?= [4]
    , "localMaxima1" ~: localMaxima [1,2,3,4,5] ~?= []
    ]
  return ()
