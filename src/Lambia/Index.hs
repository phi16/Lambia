{-# LANGUAGE OverloadedStrings #-}

module Lambia.Index where

import Prelude hiding (lookup)
import Control.Monad.State.CPS
import Control.Monad.Trans.Except
import Control.Monad
import Control.Applicative hiding (empty)
import Data.ByteString hiding (append,empty,reverse)
import qualified Data.ByteString as B
import Data.Map.Strict hiding (split)
import Data.Traversable
import qualified Data.Sequence as S

import Lambia.Types

data Status = Status (S.Seq ByteString) Save Save
type Local a = ExceptT ByteString (State Status) a

nil :: Save
nil = Save empty

ini :: Status
ini = Status S.empty nil nil

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
append q v e (Save s) = Save <$> case S.viewl q of
  S.EmptyL -> do
    let u = lookup v s
    case u of
      Just w -> case snd w of
        Just _ -> throwE $ "Duplicate variable : "`B.append`v
        Nothing -> return $ insert v (fst w,Just e) s
      Nothing -> return $ insert v (nil,Just e) s
  x S.:< xs -> case lookup x s of
    Just (p,t) -> do
      u <- append xs v e p
      return $ insert x (u,t) s
    Nothing -> do
      u <- append xs v e nil
      return $ insert x (u,Nothing) s

match :: [ByteString] -> Save -> Maybe Save
match [] e = Just e
match (x:xs) (Save e) = case lookup x e of
  Just (y,_) -> match xs y
  Nothing -> Nothing

meld :: Entity -> Entity -> Entity
meld (Save a,v) (Save b,w) = let
    c = unionWith meld a b
    x = v <|> w
  in (Save c,x)

merge :: [ByteString] -> Save -> Save -> Save
merge [] (Save l) (Save r) = Save $ unionWith meld l r
merge (x:xs) l (Save r) = Save $ case lookup x r of
  Just (e,v) -> insert x (merge xs l e, v) r
  Nothing -> insert x (merge xs l nil, Nothing) r

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
    Nothing -> return $ merge us nil l -- throwE $ "Not a scope name : "`B.append`u
  put $ Status b g r

ixExpr :: Expr -> Local Lambda
ixExpr (Expr decls t) = return $ Index 0

