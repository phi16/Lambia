{-# LANGUAGE OverloadedStrings #-}

module Lambia.Types (
  Term(..),Expr(..),Declare(..),Source(..),
  Lambda(..),Combi(..),Entity,Save(..),Status(..),
  Store(..),Syn(..),cToL,sToC) where

import Data.Char (ord,chr)
import Data.Map.Strict hiding (map)
import Data.ByteString.Char8 (ByteString)

data Term = Abst [ByteString] Expr | Apply Term Term | Wrap Expr | Var ByteString deriving Show
data Expr = Expr [Declare] Term deriving Show
data Declare = Decl ByteString (Maybe ByteString) Expr | Scope Bool ByteString [Declare] | Open ByteString deriving Show
data Source = Source [Declare] (Maybe Expr) deriving Show

data Lambda = Lambda Lambda | App Lambda Lambda | Index Int | Prim ByteString
data Combi = C | B | I | S | K | A Combi Combi | P ByteString
data Syn s = Lm (Syn s) | Ap (Syn s) (Syn s) | Ix Int | Pr ByteString | Og s
type Entity a = (Save a, Maybe (a, Maybe ByteString))
newtype Save a = Save (Map ByteString (Entity a)) deriving Show

data Status a = Status (Save a) (Save a)

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

instance Show Combi where
  show l = np l "" where
    ch = (:)
    si C = ch 'C'
    si B = ch 'B'
    si I = ch 'I'
    si S = ch 'S'
    si K = ch 'K'
    si (A a b) = ch '(' . np a . si b . ch ')'
    si (P s) = (unpack s++)
    np (A a b) = np a . si b
    np x = si x

class Show a => Store a where
  simple :: Int -> a -> (Bool, a)
  apply :: a -> (a, [a])
  fromSyn :: Syn a -> a

cToL :: Combi -> Lambda
cToL x = let
    l = Lambda
    a = App
    i = Index
  in case x of
    S -> l $ l $ l $ a (a (i 2) $ i 0) $ a (i 1) $ i 0
    K -> l $ l $ i 1
    I -> l $ i 0
    C -> l $ l $ l $ a (a (i 2) $ i 0) $ i 1
    B -> l $ l $ l $ a (i 2) $ a (i 1) $ i 0
    A a b -> App (cToL a) $ cToL b
    P s -> Prim s

data LC = Co | Bo | Io | So | Ko | Ao LC LC | Po ByteString | Ro Int | Lo LC | Oo Combi deriving Show

sToC :: Syn Combi -> Combi
sToC x = undefined
