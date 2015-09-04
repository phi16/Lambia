module Lambia.Combi () where

import Prelude hiding (lookup)

import Lambia.Types hiding (simple,apply)
import qualified Lambia.Types as T (simple,apply)

simple :: Int -> Combi -> (Bool, Combi)
simple n l = s l n where
  s :: Combi -> Int -> (Bool, Combi)
  s e 0
    | size e == size l = (True,e)
    | size e < size l = (True,snd $ simple n e)
    | otherwise = (False,l)
  s e x = case repl e of
    (False,_) -> s e 0
    (True,e') -> s e' (x-1)
  size :: Combi -> Int
  size (A x y) = size x + size y
  size _ = 1

apply :: Combi -> (Combi, [Combi])
apply i = a i [] where
  a x xs = case repl x of
    (True, x') -> a x' (x:xs)
    (False, _) -> (x,xs)

repl :: Combi -> (Bool, Combi)
repl (A I x) = (True, snd $ repl x)
repl (A (A K x) y) = (True, snd $ repl x)
repl (A (A (A S x) y) z) = (True, A (A x z) (A y z))
repl (A (A (A C x) y) z) = (True, snd $ repl $ A (A x z) y)
repl (A (A (A B x) y) z) = (True, snd $ repl $ A x (A y z))
repl (A x y) = case repl x of
  (True, x') -> (True, A x' y)
  (False, _) -> case repl y of
    (True, y') -> (True, A x y')
    (False, _) -> (False, A x y)
repl x = (False, x)

instance Store Combi where
  simple = simple
  apply = apply
  fromSyn = sToC

