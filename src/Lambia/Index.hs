{-# LANGUAGE OverloadedStrings #-}

module Lambia.Index where

import Prelude hiding (lookup)
import Control.Monad.State.CPS
import Control.Monad.Trans.Except
import Control.Monad
import Data.ByteString hiding (append,empty,reverse)
import qualified Data.ByteString as B
import Data.Map.Strict hiding (split)
import Data.Traversable
import qualified Data.Sequence as S

import Lambia.Types

data Status = Status (S.Seq ByteString) Save Save
type Local a = ExceptT ByteString (State Status) a

ini :: Status
ini = Status S.empty empty empty

pass :: Status -> (Save, Save)
pass (Status _ a b) = (a,b)

indexing :: Source -> Either ByteString (Maybe Lambda,(Save,Save))
indexing (Source decls e) = let
    (e',s) = flip runState ini $ runExceptT $ do
      mapM_ ixDecl decls
      traverse ixExpr e
  in case e' of
    Left l -> Left l
    Right d -> Right (d,pass s)

append :: S.Seq ByteString -> ByteString -> Lambda -> Save -> Local Save
append q v e s = case S.viewl q of
  S.EmptyL  -> return $ insert v (Value e) s
  x S.:< xs -> case lookup x s of
    Just (Scoping p) -> do
      u <- append xs v e p
      return $ insert x (Scoping u) s
    Just (Value _) -> do
      u <- append xs v e empty
      return $ insert x (Scoping u) s
    Nothing -> do
      u <- append xs v e empty
      return $ insert x (Scoping u) s

match :: [ByteString] -> Save -> Maybe Save
match [] e = Just e
match (x:xs) e = case lookup x e of
  Just (Scoping y) -> match xs y
  Just (Value v) -> match xs empty
  Nothing -> Nothing

meld :: Entity -> Entity -> Entity
meld (Scoping a) (Scoping b) = Scoping $ unionWith meld a b
meld (Scoping a) (Value r) = Scoping a
meld (Value r) _ = Value r

merge :: [ByteString] -> Save -> Save -> Save
merge [] l r = unionWith meld l r
merge (x:xs) l r = case lookup x r of
  Just (Scoping e) -> insert x (Scoping $ merge xs l e) r
  Just (Value v) -> insert x (Scoping $ merge xs l empty) r
  Nothing -> insert x (Scoping $ merge xs l empty) r

ixDecl :: Declare -> Local ()
ixDecl (Decl str e) = do
  Status b g l <- get
  m <- ixExpr e
  g' <- append b str m g
  l' <- append b str m l
  l'' <- append S.empty str m l
  put $ Status b g' l''
ixDecl (Scope False str ds) = do
  Status b g l <- get
  put $ Status (b S.|> str) g l
  mapM_ ixDecl ds
  Status _ g' _ <- get
  put $ Status b g' l
ixDecl (Scope True str ds) = do
  Status b g l <- get
  put $ Status (b S.|> str) g l
  mapM_ ixDecl ds
  Status _ g' l' <- get
  put $ Status b g' l'
ixDecl (Open u) = do
  Status b g l <- get
  let
    toW8 = toEnum . fromEnum
    us = split (toW8 '.') u
  r <- case match us g of
    Just e -> return $ merge us e l
    Nothing -> return $ merge us empty l -- throwE $ "Not a scope name : "`B.append`u
  put $ Status b g r

ixExpr :: Expr -> Local Lambda
ixExpr (Expr decls t) = return $ Index 0

