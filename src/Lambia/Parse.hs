{-# LANGUAGE OverloadedStrings #-}

module Lambia.Parse where

import Prelude hiding (takeWhile)
import Control.Monad
import Control.Applicative
import Data.Char (ord)
import Data.Word (Word8)
import Data.ByteString (ByteString, singleton, cons, pack)
import Data.Attoparsec.ByteString

{-

Source := Declare* Expr?
Declare := DeclName = Expr | Name { Declare* }
Expr := { Declare* } Term | Term
Term := Term+
ETerm := \ Args . Expr | Var | (Expr)
Args := VarName+
VarName := 'a-z' | Name
DeclName := ('a-zA-Z')+
Name := 'A-Z'('a-zA-Z')*
Var := VarName | (Name .)* Name

-}

data Term = Abst [ByteString] Expr | Apply Term Term | Wrap Expr | Var ByteString deriving Show
data Expr = Expr [Declare] Term deriving Show
data Declare = Decl ByteString Expr | Scope ByteString [Declare] deriving Show
data Source = Source [Declare] (Maybe Expr) deriving Show

none :: Parser ()
none = skipWhile (\x -> fromEnum x == fromEnum ' ')

char8 :: Char -> Parser Word8
char8 c = word8 $ fromIntegral $ ord c

noneWrap :: Parser a -> Parser a
noneWrap a = do
  none
  x <- a
  none
  return x

spaces :: Parser ()
spaces = char8 ' ' >> none

lf :: Parser ()
lf = do
  many1' $ do
    none
    choice $ map char8 ['\n','\r']
  return ()

source :: Parser Source
source = do
  lf <|> return ()
  ds <- many' $ decl
  e <- choice [Just <$> expr, return Nothing]
  many' lf
  endOfInput
  return $ Source ds e

decl :: Parser Declare
decl = (do
    none
    n <- declName
    noneWrap $ char8 '='
    e <- expr
    lf
    return $ Decl n e
  ) <|> (do
    none
    n <- name
    noneWrap $ char8 '{'
    lf
    ds <- many' decl
    none
    char8 '}'
    lf
    return $ Scope n ds
  )

expr :: Parser Expr
expr = (do
    noneWrap $ char8 '{'
    lf
    ds <- many' decl
    char8 '}'
    none
    e <- term
    return $ Expr ds e
  ) <|> (do
    t <- term
    return $ Expr [] t
  )

term :: Parser Term
term = do
  ts <- many1 eTerm
  return $ foldl1 Apply ts

eTerm :: Parser Term
eTerm = noneWrap $ (do
    char8 '\\'
    ss <- noneWrap args
    char8 '.'
    t <- noneWrap expr
    return $ Abst ss t
  ) <|> (do
    char8 '('
    t <- noneWrap expr
    char8 ')'
    return $ Wrap t
  ) <|> (do
    v <- var
    return $ Var v
  )

declName :: Parser ByteString
declName = do
  h <- satisfy $ inClass "a-zA-Z0-9"
  hs <- takeWhile $ inClass "a-zA-Z0-9"
  return $ cons h hs

name :: Parser ByteString
name = do
  h <- satisfy $ inClass "A-Z0-9"
  hs <- takeWhile $ inClass "a-zA-Z0-9"
  return $ cons h hs

var :: Parser ByteString
var = (do
  str <- match $ do
    many' $ name >> char8 '.'
    declName
  return $ fst str) <|> declName

args :: Parser [ByteString]
args = many1 $ (do
    s <- name
    x <- peekWord8
    if x == Just (toEnum $ fromEnum $ ord '.')
      then return ()
      else spaces
    return s
  ) <|> (do
    fmap singleton $ satisfy (inClass "a-z")
  )
