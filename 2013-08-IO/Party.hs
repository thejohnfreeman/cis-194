{-# OPTIONS_GHC -fno-warn-orphans #-}

module Party where

import Data.Tree
import Employee

glCons :: Employee -> GuestList -> GuestList
glCons e (GL es f) = GL (e:es) (f + empFun e)

instance Monoid GuestList where
  mempty = GL [] 0
  mappend (GL les lf) (GL res rf) = GL (les ++ res) (lf + rf)

moreFun :: GuestList -> GuestList -> GuestList
moreFun a b = if glFun a >= glFun b then a else b

treeFold :: (b -> a -> b) -> b -> Tree a -> b
treeFold f z t = let z' = z `f` rootLabel t
                 in z' `seq` foldl (treeFold f) z' (subForest t)

-- (best with boss, best without boss)
nextLevel :: Employee -> [(GuestList, GuestList)] -> (GuestList, GuestList)
nextLevel boss gls = (glCons boss $ mconcat $ map snd gls, mconcat $ map fst gls)

maxFun :: Tree Employee -> GuestList
maxFun = uncurry moreFun . maxFun'
  where maxFun' (Node boss emps) = nextLevel boss $ map maxFun' emps

pp :: GuestList -> String
pp gl = unlines $ ("Total fun: " ++ show (glFun gl)) : map empName (glEmps gl)

main :: IO ()
main = do
  company <- read <$> readFile "company.txt"
  putStr $ pp $ maxFun company
