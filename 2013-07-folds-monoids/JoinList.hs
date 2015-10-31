{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module JoinList where

import Buffer
import Data.List
import Data.Monoid
import Editor
import Scrabble
import Sized
import Test.QuickCheck

(!!?) :: [a] -> Int -> Maybe a
[] !!? _ = Nothing
_ !!? i | i < 0 = Nothing
(x:__) !!? 0 = Just x
(_:xs) !!? i = xs !!? (i-1)

data JoinList m a
  = Empty
  | Single m a
  | Append m (JoinList m a) (JoinList m a)
  deriving (Eq, Show)

tag :: Monoid m => JoinList m a -> m
tag Empty = mempty
tag (Single m _) = m
tag (Append m _ _) = m

(+++) :: Monoid m => JoinList m a -> JoinList m a -> JoinList m a
Empty +++ b = b
a +++ Empty = a
a +++ b = Append (tag a <> tag b) a b

jlToList :: JoinList m a -> [a]
jlToList Empty = []
jlToList (Single _ a) = [a]
jlToList (Append _ l r) = jlToList l ++ jlToList r

indexJ :: (Sized b, Monoid b) => Int -> JoinList b a -> Maybe a
indexJ 0 (Single _ a) = Just a
indexJ n (Append _ l r)
  | n' >= 0   = indexJ n' r
  | otherwise = indexJ n l
  where n' = n - getSize (size (tag l))
indexJ _ _ = Nothing

dropJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
dropJ n jl | n <= 0 = jl
dropJ n (Append _ l r)
  | n' >= 0   = dropJ n' r
  | otherwise = dropJ n l +++ r
  where n' = n - getSize (size (tag l))
dropJ _ _ = Empty

takeJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
takeJ n _ | n <= 0 = Empty
takeJ n (Append _ l r)
  | n' > 0    = l +++ takeJ n' r
  | otherwise = takeJ n l
  where n' = n - getSize (size (tag l))
takeJ _ jl = jl

foldrJ :: (a -> b -> b) -> b -> JoinList m a -> b
foldrJ _ z Empty = z
foldrJ f z (Single _ x) = f x z
foldrJ f z (Append _ l r) = foldrJ f (foldrJ f z r) l

instance Arbitrary a => Arbitrary (JoinList Size a) where
  arbitrary = sized $ \n -> if
    | n == 0 -> return Empty
    | n == 1 -> Single (Size 1) <$> arbitrary
    | otherwise -> do
      l <- resize (n `div` 2) arbitrary
      r <- resize (n `div` 2) arbitrary
      return $ l +++ r

instance Arbitrary Size where
  arbitrary = Size <$> arbitrary

--scoreLine :: String -> JoinList Score String
--scoreLine s = Single (scoreString s) s

-- Scores = Scrabble value
-- Size = numer of lines
type JoinListBuffer = (JoinList (Score, Size) String)
instance Buffer JoinListBuffer where
  toString = concat . jlToList
  fromString = foldl' (+++) Empty . map single . lines
    where single "" = Empty
          single s = Single (scoreString s, 1) s
  line = indexJ
  replaceLine n s jl = takeJ n jl +++ fromString s +++ dropJ (n+1) jl
  numLines = foldrJ (const (1+)) 0
  value = fromIntegral . fst . tag

main :: IO ()
main = do
  sequence_
    [ quickCheck propIndex
    , quickCheck propDrop
    , quickCheck propTake
    , quickCheck propFold
    ]
  runEditor editor (fromString "Welcome!" :: JoinListBuffer)
    where
      propIndex :: Int -> JoinList Size Int -> Bool
      propIndex i jl = indexJ i jl == jlToList jl !!? i
      propDrop :: Int -> JoinList Size Int -> Bool
      propDrop n jl = jlToList (dropJ n jl) == drop n (jlToList jl)
      propTake :: Int -> JoinList Size Int -> Bool
      propTake n jl = jlToList (takeJ n jl) == take n (jlToList jl)
      propFold :: Int -> JoinList Size Int -> Bool
      propFold n jl = foldrJ (+) n jl == foldr (+) n (jlToList jl)
