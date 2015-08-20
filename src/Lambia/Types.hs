{-# LANGUAGE OverloadedStrings #-}

module Lambia.Types where

import Prelude hiding (takeWhile)
import Control.Monad
import Control.Applicative
import Data.Char (ord)
import Data.Word (Word8)
import Data.Map.Strict (Map)
import Data.ByteString (ByteString, singleton, cons, pack)
import Data.Attoparsec.ByteString

data Term = Abst [ByteString] Expr | Apply Term Term | Wrap Expr | Var ByteString deriving Show
data Expr = Expr [Declare] Term deriving Show
data Declare = Decl ByteString Expr | Scope Bool ByteString [Declare] | Open ByteString deriving Show
data Source = Source [Declare] (Maybe Expr) deriving Show

data Lambda = Lambda Lambda | App Lambda Lambda | Index Int | Prim ByteString deriving Show
data Entity = Scoping Save | Value Lambda deriving Show
type Save = Map ByteString Entity

