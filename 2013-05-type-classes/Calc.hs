{-# LANGUAGE FlexibleInstances #-}

module Calc where

import ExprT
import Parser
import qualified StackVM as VM
import Test.HUnit

eval :: ExprT -> Integer
eval (Lit i) = i
eval (Add a b) = eval a + eval b
eval (Mul a b) = eval a * eval b

evalStr :: String -> Maybe Integer
evalStr s = do
  e <- parseExp Lit Add Mul s
  return $ eval e

class Expr a where
  lit :: Integer -> a
  add :: a -> a -> a
  mul :: a -> a -> a

instance Expr ExprT where
  lit = Lit
  add = Add
  mul = Mul

instance Expr Integer where
  lit = id
  add = (+)
  mul = (*)

instance Expr Bool where
  lit = (>0)
  add = (||)
  mul = (&&)

newtype MinMax = MinMax Integer deriving (Eq, Show)

instance Expr MinMax where
  lit = MinMax
  add (MinMax a) (MinMax b) = MinMax $ max a b
  mul (MinMax a) (MinMax b) = MinMax $ min a b

newtype Mod7 = Mod7 Integer deriving (Eq, Show)

instance Expr Mod7 where
  lit = Mod7
  add (Mod7 a) (Mod7 b) = Mod7 $ (a + b) `mod` 7
  mul (Mod7 a) (Mod7 b) = Mod7 $ (a * b) `mod` 7

testExp :: Expr a => Maybe a
testExp = parseExp lit add mul "(3 * -4) + 5"

instance Expr VM.Program where
  lit i = [VM.PushI i]
  add a b = a ++ b ++ [VM.Add]
  mul a b = a ++ b ++ [VM.Mul]

compile :: String -> Maybe VM.Program
compile = parseExp lit add mul

main :: IO ()
main = do
  _ <- runTestTT $ test
    [ "eval1" ~: eval (Mul (Add (Lit 2) (Lit 3)) (Lit 4)) ~?= 20
    , "evalStr1" ~: evalStr "(2+3)*4" ~?= Just 20
    , "evalStr2" ~: evalStr "2+3*4" ~?= Just 14
    , "evalStr3" ~: evalStr "2+3*" ~?= Nothing
    , "instance1" ~: (mul (add (lit 2) (lit 3)) (lit 4) :: ExprT)
                  ~?= Mul (Add (Lit 2) (Lit 3)) (Lit 4)
    , "testInteger" ~: (testExp :: Maybe Integer) ~?= Just (-7)
    , "testBool" ~: (testExp :: Maybe Bool) ~?= Just True
    , "testMM" ~: (testExp :: Maybe MinMax) ~?= Just (MinMax 5)
    , "testSat" ~: (testExp :: Maybe Mod7) ~?= Just (Mod7 0)
    , "stack1" ~: maybe (Left "parse error") VM.stackVM (testExp :: Maybe VM.Program) ~?= Right (VM.IVal (-7))
    , "stack2" ~: maybe (Left "parse error") VM.stackVM (compile "(3 * -4) + 5") ~?= Right (VM.IVal (-7))
    ]
  return ()
