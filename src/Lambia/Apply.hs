module Lambia.Apply where

import Prelude hiding (lookup)
import Data.Map

import Lambia.Types

apply :: Lambda -> (Lambda, [Lambda])
apply l = case beta l 0 empty of
  (False,l',_) -> (l,[l])
  (True,l',_) -> let
      (x,xy) = apply l'
    in (x,l:xy)

beta :: Lambda -> Int -> Map Int Int -> (Bool, Lambda, Map Int Int)
beta (Lambda l) d p = let
    (b,l',m) = beta l (d+1) p
    u = delete d m
  in case lookup d m of
    Just 1 -> case l' of
      App y (Index 0) -> (True,decr 0 y,u)
      _ -> (b,Lambda l',u)
    _ -> (b,Lambda l',u)
beta (App l r) d p = case l of
  Lambda u -> (True,replace u 0 r,p)
  _        -> case beta l d p of
    (True,l',m) -> (True,App l' r,m)
    (False,_,m) -> case beta r d m of
      (True,r',m') -> (True,App l r',m')
      (False,_,m') -> (False,App l r,m')
beta (Index x) d p = (False, Index x, insertWith (+) (d-x) 1 p)

decr :: Int -> Lambda -> Lambda
decr x (Lambda l) = Lambda $ decr (x+1) l
decr x (App l r) = App (decr x l) (decr x r)
decr x (Index t)
  | t > x     = Index (t-1)
  | otherwise = Index t

lift :: Int -> Int -> Lambda -> Lambda
lift x y (Lambda l) = Lambda $ lift x (y+1) l
lift x y (App l r) = App (lift x y l) (lift x y r)
lift x y (Index t)
  | t < y = Index t
  | otherwise = Index (t+x)

replace :: Lambda -> Int -> Lambda -> Lambda
replace (Lambda l) i r = Lambda $ replace l (i+1) r
replace (App m n) i r = App (replace m i r) (replace n i r)
replace (Index t) i r
  | t > i     = Index (t-1)
  | t == i    = lift t 0 r
  | otherwise = Index t

