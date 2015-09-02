{-# LANGUAGE OverloadedStrings #-}

module Lambia.Index (nil, indexing, append, ixDecl, ixExpr, Indexed) where

import Prelude hiding (lookup, foldr)
import Control.Monad.State.Strict
import Control.Monad.Trans.Except
import Control.Applicative hiding (empty)
import Data.Char
import Data.ByteString.Char8 hiding (append,empty,reverse,foldr,last,elemIndex,head,length)
import qualified Data.ByteString.Char8 as B
import Data.List (elemIndex)
import Data.Map.Strict hiding (split)
import Data.Traversable
import Data.Maybe (isJust, fromMaybe)

import Lambia.Types

type Local s a = ExceptT ByteString (State (Status s)) a
type Indexed s = Either ByteString (Maybe s,(Save s,Save s))

nil :: Save s
nil = Save empty

ini :: Status s
ini = Status nil nil

pass :: Status s -> (Save s, Save s)
pass (Status a b) = (a,b)

indexing :: Store s => Source -> Indexed s
indexing (Source decls e) = let
    (e',s) = flip runState ini $ runExceptT $ do
      mapM_ ixDecl decls
      traverse ixExpr e
  in case e' of
    Left l -> Left l
    Right d -> Right (d,pass s)

append :: ByteString -> Maybe ByteString -> s -> Save s -> Save s
append v sc e (Save s) = Save $ let
    u = lookup v s
  in case u of
    Just w  -> insert v (fst w,Just (e,sc)) s
    Nothing -> insert v (nil,Just (e,sc)) s

match :: [ByteString] -> Save s -> Maybe (Entity s)
match [] e = Nothing
match [x] (Save e) = lookup x e
match (x:xs) (Save e) = case lookup x e of
  Just (y,_) -> match xs y
  Nothing -> Nothing

merge :: [ByteString] -> Save s -> Save s -> Local s (Save s)
merge [] (Save l) (Save r) = do
  let
    meld :: ByteString ->
            Entity s ->
            Entity s ->
            Maybe (Either ([ByteString] -> [ByteString]) (Entity s))
    meld key (Save a,v) (Save b,w) = Just $ let
        c = mu a b
        c' = mapMaybe right c
        x = v <|> w
      in case j c of
        Just e -> Left e
        Nothing -> case isJust v && isJust w of
          False -> Right (Save c',x)
          True  -> case (v,w) of
            (Just (_,vs), Just (_,ws))
              | vs == ws  -> Right (Save c',x)
              | otherwise -> let
                  i = fromMaybe "[Outer]"
                in Left (B.concat ["{",i vs,"|",i ws,"}.",key]:)
    mu = mergeWithKey meld (fmap Right) (fmap Right)
    u = mu l r
    j = foldr f Nothing where
      f (Left x) (Just xs) = Just $ x.xs
      f (Left x) Nothing = Just x
      f (Right _) (Just xs) = Just xs
      f (Right m) Nothing = Nothing
    right :: Either ([ByteString] -> [ByteString]) (Entity s) -> Maybe (Entity s)
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

ixDecl :: Store s => Declare -> Local s ()
ixDecl (Decl str scope e) = do
  Status g l <- get
  m <- ixExpr e
  let
    g' = if isLower $ B.head str
      then g
      else append str scope m g
    l' = append str scope m l
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
          Just (v',sc) 
            | length us > 1 -> (append n sc v' l', append n sc v' g')
            | otherwise -> (l',g')
          Nothing -> (l',g')
      put $ Status g'' l''
    Nothing -> if head us == "Primitive"
      then return ()
      else throwE $ "Not a scope name : "`B.append`u

ixExpr :: Store s => Expr -> Local s s
ixExpr e = snd . simple 100 . fromSyn <$> iE [] e

iE :: Store s => [ByteString] -> Expr -> Local s (Syn s)
iE us (Expr decls t) = do
  s <- get
  forM_ decls $ \(Decl str sc e) -> do
    Status g l <- get
    m <- iE us e
    let
      g' = append str Nothing (fromSyn m) g
      l' = append str Nothing (fromSyn m) l
    put $ Status g' l'
  i <- iT us t
  put s
  return i

iT :: Store s => [ByteString] -> Term -> Local s (Syn s)
iT us (Abst args e) = d args <$> iE (reverse args ++ us) e where
  d (x:xs) = Lm . d xs
  d [] = id
iT us (Apply a b) = do
  x <- iT us a
  y <- iT us b
  return $ Ap x y
iT us (Wrap e) = iE us e
iT us (Var v) = case elemIndex v us of
  Just u -> return $ Ix u
  Nothing -> do
    Status _ l <- get
    let
      us = split '.' v
      err = throwE $ "Not in scope : "`B.append`v
      -- search :: [ByteString] -> Save s -> Local s s
      search [] s = err
      search [x] (Save s) = case lookup x s of
        Just (_,Just (e,_)) -> return e
        _ -> err
      search (x:xs) (Save s) = case lookup x s of
        Just (s',_) -> search xs s'
        _ -> err
    if head us == "Primitive"
      then return $ Pr v
      else Og <$> search us l

