{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-# OPTIONS_GHC -fno-warn-missing-methods #-}
{-# LANGUAGE FlexibleInstances #-}

module Fibonacci where

fib :: Integer -> Integer
fib 0 = 0
fib 1 = 1
fib n = fib (n-1) + fib (n-2)

fibs1 :: [Integer]
fibs1 = map fib [0..]

fibs2 :: [Integer]
fibs2 = 0 : 1 : zipWith (+) fibs2 (drop 1 fibs2)

data Stream a = Cons a (Stream a)

instance Show a => Show (Stream a) where
  show = show . take 10 . streamToList

streamToList :: Stream a -> [a]
streamToList (Cons x xs) = x : streamToList xs

streamRepeat :: a -> Stream a
streamRepeat x = Cons x $ streamRepeat x

streamMap :: (a -> b) -> Stream a -> Stream b
streamMap f (Cons x xs) = Cons (f x) $ streamMap f xs

streamFromSeed :: (a -> a) -> a -> Stream a
streamFromSeed f x = Cons x $ streamFromSeed f (f x)

nats :: Stream Integer
nats = streamFromSeed (+1) 0

interleaveStreams :: Stream a -> Stream a -> Stream a
interleaveStreams (Cons x xs) ys = Cons x $ interleaveStreams ys xs

ruler :: Stream Integer
ruler = ruler' 0
  where ruler' n = interleaveStreams (streamRepeat n) $ ruler' (n+1)

x :: Stream Integer
x = Cons 0 $ Cons 1 $ streamRepeat 0

instance Num (Stream Integer) where
  fromInteger n = Cons n $ streamRepeat 0
  negate (Cons c cs) = Cons (-c) $ negate cs
  (Cons a as) + (Cons b bs) = Cons (a+b) $ as + bs
  (Cons a as') * bs@(Cons b bs') = Cons (a*b) $ streamMap (*a) bs' + (as' * bs)

instance Fractional (Stream Integer) where
  (Cons a as) / (Cons b bs) = q where
    q = Cons (a `div` b) $ streamMap (`div` b) $ as - q * bs

fibs3 :: Stream Integer
fibs3 = x / (1 - x - x^(2 :: Integer))

-- Matrix tl tr bl br
data Matrix = Matrix Integer Integer Integer Integer

instance Num Matrix where
  fromInteger n = Matrix n 0 0 n
  negate (Matrix a b c d) = Matrix (-a) (-b) (-c) (-d)
  (Matrix a b c d) + (Matrix w x y z) = Matrix (a+w) (b+x) (c+y) (d+z)
  (Matrix a b c d) * (Matrix w x y z) = Matrix
    (a*w + b*y) (c*w + d*y) (a*x + b*z) (c*x + d*z)

fib4 :: Integer -> Integer
fib4 0 = 0
fib4 n = case (Matrix 1 1 1 0) ^ (n-1) of Matrix x _ _ _ -> x
