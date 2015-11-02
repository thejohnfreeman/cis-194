{- CIS 194 HW 11
   due Monday, 8 April
-}

module SExpr where

import AParser
import Control.Applicative
import Data.Char
import Test.HUnit

------------------------------------------------------------
--  1. Parsing repetitions
------------------------------------------------------------

zeroOrMore :: Parser a -> Parser [a]
zeroOrMore p = oneOrMore p <|> pure []

oneOrMore :: Parser a -> Parser [a]
oneOrMore p = (:) <$> p <*> zeroOrMore p

------------------------------------------------------------
--  2. Utilities
------------------------------------------------------------

spaces :: Parser String
spaces = zeroOrMore $ satisfy isSpace

ident :: Parser String
ident = (:) <$> satisfy isAlpha <*> zeroOrMore (satisfy isAlphaNum)

------------------------------------------------------------
--  3. Parsing S-expressions
------------------------------------------------------------

-- An "identifier" is represented as just a String; however, only
-- those Strings consisting of a letter followed by any number of
-- letters and digits are valid identifiers.
type Ident = String

-- An "atom" is either an integer value or an identifier.
data Atom = N Integer | I Ident
  deriving Show

-- An S-expression is either an atom, or a list of S-expressions.
data SExpr = A Atom
           | Comb [SExpr]
  deriving Show

atom :: Parser Atom
atom = N <$> posInt
   <|> I <$> ident

sexpr :: Parser SExpr
sexpr = A <$> atom
    <|> Comb <$> (char '(' *> zeroOrMore (spaces *> sexpr) <* spaces <* char ')')

parseSExpr :: Parser SExpr
parseSExpr = spaces *> sexpr <* spaces

main :: IO ()
main = do
  _ <- runTestTT $ test
    [ "zeroOrMore1" ~: runParser (zeroOrMore (satisfy isUpper)) "ABCdEfgH" ~?= Just ("ABC","dEfgH")
    , "zeroOrMore2" ~: runParser (zeroOrMore (satisfy isUpper)) "abcdeFGh" ~?= Just ("","abcdeFGh")
    , "oneOrMore1" ~: runParser (oneOrMore (satisfy isUpper)) "ABCdEfgH" ~?= Just ("ABC","dEfgH")
    , "oneOrMore2" ~: runParser (oneOrMore (satisfy isUpper)) "abcdeFGh" ~?= Nothing
    , "ident1" ~: runParser ident "foobar baz" ~?= Just ("foobar"," baz")
    , "ident2" ~: runParser ident "foo33fA" ~?= Just ("foo33fA","")
    , "ident3" ~: runParser ident "2bad" ~?= Nothing
    , "ident4" ~: runParser ident "" ~?= Nothing
    ]
  return ()
