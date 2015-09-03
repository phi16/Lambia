module Lambia.Combi () where

import Prelude hiding (lookup)
import Data.Map

import Lambia.Types hiding (simple,apply)
import qualified Lambia.Types as T (simple,apply)

simple :: Int -> Combi -> (Bool, Combi)
simple = undefined

apply :: Combi -> (Combi, [Combi])
apply i = a i [] where
  a x xs = case repl x of
    (True, x') -> a x' (x:xs)
    (False, _) -> (x,xs)

repl :: Combi -> (Bool, Combi)
repl (A I x) = (True, x)
repl (A (A K x) y) = (True, x)
repl (A (A (A S x) y) z) = (True, A (A x z) (A y z))
repl (A (A (A C x) y) z) = (True, A (A x z) y)
repl (A (A (A B x) y) z) = (True, A x (A y z))
repl (A x y) = case repl x of
  (True, x') -> (True, A x' y)
  (False, _) -> case repl y of
    (True, y') -> (True, A x y')
    (False, _) -> (False, A x y)
repl x = (False, x)

instance Store Combi where
  simple = \x y -> (False,y) -- undefined
  apply = apply
  fromSyn = sToC

