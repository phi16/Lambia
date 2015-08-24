{-# LANGUAGE OverloadedStrings #-}

module Lambia.Index where

import Prelude hiding (lookup, foldr)
import Control.Monad.State.CPS
import Control.Monad.Trans.Except
import Control.Monad
import Control.Applicative hiding (empty)
import Data.Char
import Data.ByteString.Char8 hiding (append,empty,reverse,foldr,last,elemIndex,head)
import qualified Data.ByteString.Char8 as B
import Data.List (elemIndex)
import Data.Map.Strict hiding (split)
import Data.Traversable
import Data.Maybe (isJust)
import qualified Data.Sequence as S

import Lambia.Types
import Lambia.Apply

data Status = Status Save Save
type Local a = ExceptT ByteString (State Status) a

nil :: Save
nil = Save empty

ini :: Status
ini = Status nil nil

pass :: Status -> (Save, Save)
pass (Status a b) = (a,b)

indexing :: Source -> Either ByteString (Maybe Lambda,(Save,Save))
indexing (Source decls e) = let
    (e',s) = flip runState ini $ runExceptT $ do
      mapM_ ixDecl decls
      traverse ixExpr e
  in case e' of
    Left l -> Left l
    Right d -> Right (d,pass s)

append :: ByteString -> Lambda -> Save -> Save
append v e (Save s) = Save $ let
    u = lookup v s
  in case u of
    Just w  -> insert v (fst w,Just e) s
    Nothing -> insert v (nil,Just e) s

match :: [ByteString] -> Save -> Maybe Entity
match [] e = Nothing
match [x] (Save e) = lookup x e
match (x:xs) (Save e) = case lookup x e of
  Just (y,_) -> match xs y
  Nothing -> Nothing

merge :: [ByteString] -> Save -> Save -> Local Save
merge [] (Save l) (Save r) = do
  let
    meld :: ByteString ->
            Entity ->
            Entity ->
            Maybe (Either ([ByteString] -> [ByteString]) Entity)
    meld key (Save a,v) (Save b,w) = Just $ let
        c = mu a b
        c' = mapMaybe right c
        x = v <|> w
      in case j c of
        Just e -> Left e
        Nothing -> case isJust v && isJust w of
          False -> Right (Save c',x)
          True  -> Left (key:)
    mu = mergeWithKey meld (fmap Right) (fmap Right)
    u = mu l r
    j = foldr f Nothing where
      f (Left x) (Just xs) = Just $ x.xs
      f (Left x) Nothing = Just x
      f (Right _) (Just xs) = Just xs
      f (Right m) Nothing = Nothing
    right :: Either ([ByteString] -> [ByteString]) Entity -> Maybe Entity
    right (Left _) = Nothing
    right (Right x) = Just x
    u' = mapMaybe right u
  case j u of
    Just e  -> throwE $ B.append "Duplicate variable : " $ B.intercalate ", " $ e []
    Nothing -> return $ Save u'
merge (x:xs) l (Save r) = Save <$> case lookup x r of
  Just (e,v) -> do
    u <- merge xs l e
    return $ insert x (u,v) r
  Nothing -> do
    u <- merge xs l nil
    return $ insert x (u,Nothing) r

ixDecl :: Declare -> Local ()
ixDecl (Decl str e) = do
  Status g l <- get
  m <- ixExpr e
  let
    g' = if isLower $ B.head str
      then g
      else append str m g
    l' = append str m l
  put $ Status g' l'
ixDecl (Scope False str ds) = do
  Status g l <- get
  put $ Status nil l
  mapM_ ixDecl ds
  Status g' _ <- get
  l' <- merge [str] g' l
  g'' <- merge [str] g' g
  put $ Status g'' l'
ixDecl (Scope True str ds) = do
  Status g l <- get
  put $ Status nil l
  mapM_ ixDecl ds
  Status g' l' <- get
  l'' <- merge [str] g' l'
  g'' <- merge [str] g' g
  g''' <- merge [] g' g''
  put $ Status g''' l''
ixDecl (Open u) = do
  Status g l <- get
  let
    us = split '.' u
    r = match us l <|> match us g
  case r of
    Just (e,v) -> do
      let n = last us
      l' <- merge [] e l
      g' <- merge [] e g
      let
        (l'',g'') = case v of
          Just v' -> (append n v' l', append n v' g')
          Nothing -> (l',g')
      put $ Status g'' l''
    Nothing -> if head us == "Primitive"
      then return ()
      else throwE $ "Not a scope name : "`B.append`u

ixExpr :: Expr -> Local Lambda
ixExpr e = simple <$> iE [] e

iE :: [ByteString] -> Expr -> Local Lambda
iE us (Expr decls t) = do
  s <- get
  forM_ decls $ \(Decl str e) -> do
    Status g l <- get
    m <- iE us e
    let
      g' = append str m g
      l' = append str m l
    put $ Status g' l'
  i <- iT us t
  put s
  return i

iT :: [ByteString] -> Term -> Local Lambda
iT us (Abst args e) = d args <$> iE (reverse args ++ us) e where
  d (x:xs) = Lambda . d xs
  d [] = id
iT us (Apply a b) = App <$> iT us a <*> iT us b
iT us (Wrap e) = iE us e
iT us (Var v) = case elemIndex v us of
  Just u -> return $ Index u
  Nothing -> do
    Status _ l <- get
    let
      us = split '.' v
      err = throwE $ "Not in scope : "`B.append`v
      search :: [ByteString] -> Save -> Local Lambda
      search [] s = err
      search [x] (Save s) = case lookup x s of
        Just (_,Just e) -> return e
        _ -> err
      search (x:xs) (Save s) = case lookup x s of
        Just (s',_) -> search xs s'
        _ -> err
    if head us == "Primitive"
      then return $ Prim v
      else search us l

