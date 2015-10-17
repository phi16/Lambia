module Lambia.Unlambda () where

import Prelude hiding (lookup)

import Lambia.Types hiding (simple,apply)
import qualified Lambia.Types as T (simple,apply)

simple :: Int -> Unlambda -> (Bool, Unlambda)
simple n l = s l n where
  s :: Unlambda -> Int -> (Bool, Unlambda)
  s e 0
    | size e == size l = (True,e)
    | size e < size l = (True,snd $ simple n e)
    | otherwise = (False,l)
  s e x = case repl e of
    (False,_) -> s e 0
    (True,e') -> s e' (x-1)
  size :: Unlambda -> Int
  size (Au x y) = size x + size y
  size _ = 1

apply :: Unlambda -> (Unlambda, [Unlambda])
apply i = a i [] where
  a x xs = case repl x of
    (True, x') -> a x' (x:xs)
    (False, _) -> (x,xs)

repl :: Unlambda -> (Bool, Unlambda)
repl (Au Iu x) = (True, snd $ repl x)
repl (Au (Au Ku x) y) = (True, snd $ repl x)
repl (Au (Au (Au Su x) y) z) = (True, Au (Au x z) (Au y z))
repl (Au x y) = case repl x of
  (True, x') -> (True, Au x' y)
  (False, _) -> case repl y of
    (True, y') -> (True, Au x y')
    (False, _) -> (False, Au x y)
repl x = (False, x)

instance Store Unlambda where
  simple = simple
  apply = apply
  fromSyn = sToU

