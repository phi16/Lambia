{-# LANGUAGE OverloadedStrings #-}

module Lambia.Types (
  Term(..),Expr(..),Declare(..),Source(..),
  Lambda(..),Combi(..),Unlambda(..),Entity,Primitive,Save(..),Status(..),
  Store(..),Syn(..),sToC,sToU) where

import Prelude hiding (lookup)
import Data.Char (ord,chr)
import Data.Map.Strict hiding (map)
import Data.ByteString.Char8 (ByteString, pack, unpack)

data Term = Abst [ByteString] Expr | Apply Term Term | Wrap Expr | Var ByteString deriving Show
data Expr = Expr [Declare] Term deriving Show
data Declare = Decl ByteString (Maybe ByteString) Expr | Scope Bool ByteString [Declare] | Open ByteString deriving Show
data Source = Source [Declare] (Maybe Expr) deriving Show

data Lambda = Lambda Lambda | App Lambda Lambda | Index Int | Prim ByteString
data Combi = C | B | I | S | K | V Int | A Combi Combi | P ByteString
data Unlambda = Su | Ku | Iu | Vu Int | Au Unlambda Unlambda | Pu ByteString
data Syn s = Lm (Syn s) | Ap (Syn s) (Syn s) | Ix Int | Pr ByteString | Og s
type Entity a = (Save a, Maybe (a, Maybe ByteString))
type Primitive a = ByteString -> Maybe (Entity a)
newtype Save a = Save (Map ByteString (Entity a), Primitive a)

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

instance Show Unlambda where
  show l = d l "" where
    d Su = ('s':)
    d Ku = ('k':)
    d Iu = ('i':)
    d (Au x y) = ('`':).d x.d y
    d (Pu s) = (unpack s++)

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

data LC = Co | Bo | Io | So | Ko | Ao LC LC | Po ByteString | Ro Int | Lo LC deriving Show

sToC :: Syn Combi -> Combi
sToC x = just $ fst $ la (toLC x) 0 empty where
  toLC :: Syn Combi -> LC
  toLC (Lm x) = Lo $ toLC x
  toLC (Ap x y) = Ao (toLC x) $ toLC y
  toLC (Ix n) = Ro n
  toLC (Pr s) = Po s
  toLC (Og i) = cToLC i
  cToLC (A x y) = Ao (cToLC x) (cToLC y)
  cToLC (P s) = Po s
  cToLC (V n) = Ro n
  cToLC C = Co
  cToLC K = Ko
  cToLC I = Io
  cToLC S = So
  cToLC B = Bo

  la :: LC -> Int -> Map Int Int -> (LC, Map Int Int)
  la (Ro x) d m = (Ro x,insertWith (+) (d-x-1) 1 m)
  la (Ao x y) d m = let
      (x',m') = la x d m
      (y',m'') = la y d m'
    in (Ao x' y',m'')
  la (Po s) d m = (Po s,m)
  la (Lo (Ro 0)) d m = (Io, delete d m)
  la (Lo (Ro n)) d m = let
      m' = insertWith (+) ((d+1)-n-1) 1 m
    in (decr 0 $ Ao Ko $ Ro n, m')
  la (Lo (Lo e)) d m = let
      (e',m') = la e (d+2) m
    in case lookup d m' of
      Nothing -> let
          (e'',_) = la (Lo e') (d+1) m
        in (decr 0 $ Ao Ko e'', delete d $ delete (d+1) m')
      Just v  -> let
          (e'',_) = la (Lo e') (d+1) m
          (e''',p) = la (Lo e'') d m
        in (e''', delete d $ delete (d+1) p)
  la (Lo (Ao x y)) d m = let
      (x',m') = la x (d+1) m
      (y',m'') = la y (d+1) m'
    in case (lookup d m', lookup d m'') of
      (Nothing,Nothing) -> (decr 0 $ Ao Ko $ Ao x' y', delete d m'')
      (Nothing,Just 1)
        | Ro 0 <- y' -> (decr 0 x', delete d m'')
      (Nothing,Just x) -> let
          (y'',_) = la (Lo y') (d+1) m'
        in (Ao (Ao Bo $ decr 0 x') y'', delete d m'')
      (Just x,Just y)
        | x == y -> let
            (x'',_) = la (Lo x') (d+1) m
          in (Ao (Ao Co x'') $ decr 0 y', delete d m'')
        | x < y -> let
            (x'',_) = la (Lo x') (d+1) m
            (y'',_) = la (Lo y') (d+1) m'
          in (Ao (Ao So x'') y'', delete d m'')
  la (Lo x) d m = (Ao Ko x, m)
  la Co d m = (Co, m)
  la Bo d m = (Bo, m)
  la So d m = (So, m)
  la Io d m = (Io, m)
  la Ko d m = (Ko, m)

  decr :: Int -> LC -> LC
  decr x (Lo l) = Lo $ decr (x+1) l
  decr x (Ao l r) = Ao (decr x l) (decr x r)
  decr x (Ro n)
    | n > x = Ro $ n-1
    | otherwise = Ro n
  decr x y = y

  just Co = C
  just Bo = B
  just Io = I
  just So = S
  just Ko = K
  just (Ao x y) = A (just x) $ just y
  just (Po s) = P s
  just (Ro n) = V n

uToL :: Unlambda -> Lambda
uToL x = let
    l = Lambda
    a = App
    i = Index
  in case x of
    Su -> l $ l $ l $ a (a (i 2) $ i 0) $ a (i 1) $ i 0
    Ku -> l $ l $ i 1
    Iu -> l $ i 0
    Au a b -> App (uToL a) $ uToL b
    Pu s -> Prim s

data LU = Ie | Se | Ke | Ae LU LU | Pe ByteString | Re Int | Le LU deriving Show

sToU :: Syn Unlambda -> Unlambda
sToU x = just $ fst $ la (toLU x) 0 empty where
  toLU :: Syn Unlambda -> LU
  toLU (Lm x) = Le $ toLU x
  toLU (Ap x y) = Ae (toLU x) $ toLU y
  toLU (Ix n) = Re n
  toLU (Pr s) = Pe s
  toLU (Og i) = cToLU i
  cToLU (Au x y) = Ae (cToLU x) (cToLU y)
  cToLU (Pu s) = Pe s
  cToLU (Vu n) = Re n
  cToLU Ku = Ke
  cToLU Iu = Ie
  cToLU Su = Se

  la :: LU -> Int -> Map Int Int -> (LU, Map Int Int)
  la (Re x) d m = (Re x,insertWith (+) (d-x-1) 1 m)
  la (Ae x y) d m = let
      (x',m') = la x d m
      (y',m'') = la y d m'
    in (Ae x' y',m'')
  la (Pe s) d m = (Pe s,m)
  la (Le (Re 0)) d m = (Ie, delete d m)
  la (Le (Re n)) d m = let
      m' = insertWith (+) ((d+1)-n-1) 1 m
    in (decr 0 $ Ae Ke $ Re n, m')
  la (Le (Le e)) d m = let
      (e',m') = la e (d+2) m
    in case lookup d m' of
      Nothing -> let
          (e'',_) = la (Le e') (d+1) m
        in (decr 0 $ Ae Ke e'', delete d $ delete (d+1) m')
      Just v  -> let
          (e'',_) = la (Le e') (d+1) m
          (e''',p) = la (Le e'') d m
        in (e''', delete d $ delete (d+1) p)
  la (Le (Ae x y)) d m = let
      (x',m') = la x (d+1) m
      (y',m'') = la y (d+1) m'
    in case (lookup d m', lookup d m'') of
      (Nothing,Nothing) -> (decr 0 $ Ae Ke $ Ae x' y', delete d m'')
      (Nothing,Just 1)
        | Re 0 <- y' -> (decr 0 x', delete d m'')
      _ -> let
          (x'',_) = la (Le x') (d+1) m
          (y'',_) = la (Le y') (d+1) m'
        in (Ae (Ae Se x'') y'', delete d m'')
  la (Le x) d m = (Ae Ke x, m)
  la Se d m = (Se, m)
  la Ie d m = (Ie, m)
  la Ke d m = (Ke, m)

  decr :: Int -> LU -> LU
  decr x (Le l) = Le $ decr (x+1) l
  decr x (Ae l r) = Ae (decr x l) (decr x r)
  decr x (Re n)
    | n > x = Re $ n-1
    | otherwise = Re n
  decr x y = y

  just Ie = Iu
  just Se = Su
  just Ke = Ku
  just (Ae x y) = Au (just x) $ just y
  just (Pe s) = Pu s
  just (Re n) = Vu n
