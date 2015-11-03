{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Risk where

import Control.Monad
import Control.Monad.Random
import Data.List

------------------------------------------------------------
-- Die values

newtype DieValue = DV { unDV :: Int } 
  deriving (Eq, Ord, Show, Num)

first :: (a -> b) -> (a, c) -> (b, c)
first f (a, c) = (f a, c)

instance Random DieValue where
  random           = first DV . randomR (1,6)
  randomR (low,hi) = first DV . randomR (max 1 (unDV low), min 6 (unDV hi))

die :: Rand StdGen DieValue
die = getRandom

------------------------------------------------------------
-- Risk

type Army = Int

data Battlefield = Battlefield { attackers :: Army, defenders :: Army }
  deriving (Show)

battle :: Battlefield -> Rand StdGen Battlefield
battle bf = do
  let atk = min 3 $ attackers bf - 1
  let def = min 2 $ defenders bf
  atks <- replicateM atk die
  defs <- replicateM def die
  let desc = sortBy $ flip compare
  let (akills, dkills) = partition id $ zipWith (>) (desc atks) (desc defs)
  return $ Battlefield (attackers bf - length dkills) (defenders bf - length akills)

invade :: Battlefield -> Rand StdGen Battlefield
invade bf
  | attackers bf < 2 || defenders bf == 0 = return bf
  | otherwise = battle bf >>= invade

successProb :: Battlefield -> Rand StdGen Double
successProb bf = do
  bfs <- replicateM 1000 $ invade bf
  return $ fromIntegral (length $ filter ((==0) . defenders) bfs) / 1000.0
