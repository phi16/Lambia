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
Declare := DeclName = Expr | Name? { Declare* }
Expr := { Declare* } Term | Term
Term := \ Args . Term | Term Term | Var | (Term)
Args := VarName+
VarName := 'a-z' | Name
DeclName := ('a-zA-Z')+
Name := 'A-Z'('a-zA-Z')*
Var := VarName | (Name .)* Name

-}

data Term = Abst [ByteString] Term | Apply Term Term | Var ByteString deriving Show
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

countSpace :: Int -> Parser Int
countSpace x = do
  replicateM x $ char8 ' '
  ss <- many' $ char8 ' '
  return $ x + length ss

justSpace :: Int -> Parser ()
justSpace x = void $ replicateM x $ char8 ' '

source :: Parser Source
source = do
  lf <|> return ()
  ds <- many' $ decl 0
  e <- choice [Just <$> expr 0, return Nothing]
  many' lf
  endOfInput
  return $ Source ds e

decl :: Int -> Parser Declare
decl x = (do
    y <- countSpace x
    n <- declName
    noneWrap $ char8 '='
    e <- expr y
    lf
    return $ Decl n e
  ) <|> (do
    y <- countSpace x
    n <- name
    noneWrap $ char8 '{'
    lf
    ds <- many' $ decl (y+1)
    none
    char8 '}'
    lf
    return $ Scope n ds
  )

expr :: Int -> Parser Expr
expr x = (do
    noneWrap $ char8 '{'
    lf
    ds <- many' $ decl (x+1)
    justSpace x
    char8 '}'
    none
    e <- term
    lf
    return $ Expr ds e
  ) <|> (do
    x <- term
    return $ Expr [] x
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
    t <- noneWrap term
    return $ Abst ss t
  ) <|> (do
    char8 '('
    t <- noneWrap term
    char8 ')'
    return t
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
