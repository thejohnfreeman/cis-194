{-# LANGUAGE ScopedTypeVariables #-}

import Data.List hiding (insert)
import Test.HUnit
import Test.QuickCheck (quickCheck)
import Test.QuickCheck.Function

fun1 :: [Integer] -> Integer
fun1 [] = 1
fun1 (x:xs)
  | even x = (x - 2) * fun1 xs
  | otherwise = fun1 xs

fun1' :: [Integer] -> Integer
fun1' = foldl' (*) 1 . map (subtract 2) . filter even

fun2 :: Integer -> Integer
fun2 n
  | n <= 1 = 0
  | even n = n + fun2 (n `div` 2)
  | otherwise = fun2 (3 * n + 1)

fun2' :: Integer -> Integer
fun2' = sum . filter even . takeWhile (>1) . iterate step
  where step n = if even n then n `div` 2 else 3 * n + 1

data Tree a = Leaf
            | Node Integer (Tree a) a (Tree a)
            deriving (Show, Eq)

foldTree :: [a] -> Tree a
foldTree = foldl' insert Leaf
  where insert Leaf x = Node 1 Leaf x Leaf
        insert (Node _ l b r) x =
          let l' = insert l x
              hl' = height l'
              r' = insert r x
              hr' = height r'
          in if hl' < hr'
          then Node (hl'+1) l' b r
          else Node (hr'+1) l b r'
        height Leaf = 0
        height (Node h _ _ _) = h

xor :: [Bool] -> Bool
xor = foldl' (/=) False

map' :: (a -> b) -> [a] -> [b]
map' f = foldr ((:) . f) []

myFoldl :: (a -> b -> a) -> a -> [b] -> a
myFoldl f z xs = foldr ((.) . flip f) id xs z

sieveSundaram :: Integer -> [Integer]
sieveSundaram n =
  let composites = [i+j+2*i*j | j <- [1..n], i <- [1..j], i+j+2*i*j <= n]
  in [2*x+1 | x <- [1..n], x `notElem` composites]

main :: IO ()
main = do
  sequence_
    [ quickCheck $ \xs -> fun1 xs == fun1' xs
    , quickCheck $ \n -> fun2 n == fun2' n
    , quickCheck $ \(Fn f :: Fun Int Int) (xs :: [Int]) -> map f xs == map' f xs
    , quickCheck $ \(xs :: [Int]) -> foldl' (+) 0 xs == myFoldl (+) 0 xs
    , quickCheck $ \(xs :: [Int]) -> foldl' (*) 1 xs == myFoldl (*) 1 xs
    , print $ foldTree "ABCDEFGH"
    ]
  _ <- runTestTT $ test
    [ "xor1" ~: xor [False, True, False] ~?= True
    , "xor2" ~: xor [False, True, False, False, True] ~?= False
    , "sieve" ~: sieveSundaram 10 ~?= [3, 5, 7, 11, 13, 17, 19]
    ]
  return ()
