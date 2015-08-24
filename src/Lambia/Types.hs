{-# LANGUAGE OverloadedStrings #-}

module Lambia.Types (
  Term(..),Expr(..),Declare(..),Source(..),
  Lambda(..),Entity,Save(..),Status(..)) where

import Data.Char (ord,chr)
import Data.Map.Strict hiding (map)
import Data.ByteString.Char8 (ByteString)

data Term = Abst [ByteString] Expr | Apply Term Term | Wrap Expr | Var ByteString deriving Show
data Expr = Expr [Declare] Term deriving Show
data Declare = Decl ByteString Expr | Scope Bool ByteString [Declare] | Open ByteString deriving Show
data Source = Source [Declare] (Maybe Expr) deriving Show

data Lambda = Lambda Lambda | App Lambda Lambda | Index Int | Prim ByteString
type Entity = (Save, Maybe Lambda)
newtype Save = Save (Map ByteString Entity) deriving Show

data Status = Status Save Save

lowerN :: Int -> Char
lowerN n = chr $ n + ord 'a'

isLambda (Lambda l) = True
isLambda _ = False

unpackLambda (n,Lambda l) = (n+1,l)

meld xs = foldl1 (.) $ map (++) xs

instance Show Lambda where
  show l = showL False l 0 [] where
    showL :: Bool -> Lambda -> Int -> String -> String
    showL b l@(Lambda _) i = let
        (n,body) = until (not.isLambda.snd) unpackLambda (0,l)
        arg = take n [lowerN i..]
      in meld ["(\\",arg,"."] . showL False body (i+n) . meld [")"]
    showL True l@(App a b) i = meld ["("] . showL False l i . meld [")"]
    showL False l@(App a b) i = let
        left = showL False a i
        right = showL True b i
      in left . meld [" "] . right
    showL b (Index t) i = meld [[lowerN (i-t-1)]]

