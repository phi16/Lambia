{-# LANGUAGE OverloadedStrings #-}

module Lambia.Parse (parseSource, parseLines) where

import Prelude hiding (concat, null)
import Control.Monad
import Control.Applicative hiding (many,optional,(<|>))
import Data.ByteString.Char8 (ByteString, singleton, snoc, pack, intercalate, concat, null)
import Text.Parsec
import Text.Parsec.ByteString

import Lambia.Types

{-

Source := Declare* Expr?
Declare := DeclName = Expr | open? Name? { Declare* } | open? ScopeName
Expr := { Declare* } Term | Term
Term := Term+
ETerm := \ Args . Expr | Var | (Expr)
Args := VarName+
VarName := 'a-z' | Name
DeclName := ('a-zA-Z')+
Name := 'A-Z'('a-zA-Z')*
ScopeName := Name . ScopeName | Name
Var := VarName | ScopeName

-}

parseSource :: String -> ByteString -> Either ByteString Source
parseSource fn s = case parse source ('[':(fn++"]")) $ s`snoc`'\n' of
  Left err -> Left $ pack $ show err
  Right e -> Right e

parseLines :: ByteString -> Either ByteString (Either Declare Expr)
parseLines s = let
    pls :: Parser (Either Declare Expr)
    pls = try (Right <$> (expr <* lf)) <|> (Left <$> decl (Just ""))
  in case parse (pls <* char '\0') "[Interactive]" $ s`snoc`'\0' of
    Left err -> Left $ pack $ show err
    Right e -> Right e

none :: Parser ()
none = void $ many $ char ' ' <|> char '\t'

noneWrap :: Parser a -> Parser a
noneWrap a = do
  none
  x <- a
  none
  return x

spaces1 :: Parser ()
spaces1 = (char ' ' <|> char '\t') >> none

lf :: Parser ()
lf = eof <|> do
  let
    l = let
        a = try $ do
          none
          string "//" <|> string ":"
          manyTill anyChar $ try $ void endOfLine <|> eof
          return ()
        b = try $ do
          none
          void endOfLine
        c = do
          spaces1
          eof
      in choice [a,b,c]
  l
  manyTill l $ try $ eof <|> notFollowedBy l
  return ()

source :: Parser Source
source = do
  try lf <|> return ()
  (ds,e) <- let
      du (xs,j) = try (do
          e <- optionMaybe expr
          lf
          return (xs [],e)
        ) <|> (do
          d <- decl $ Just ""
          du (xs . (d:), j))
    in du (id,Nothing)
  eof
  return $ Source ds e

plus :: ByteString -> Maybe ByteString -> Maybe ByteString
plus x Nothing = Nothing
plus x (Just y) = Just $ if null y then x else concat [y,".",x]

decl :: Maybe ByteString -> Parser Declare
decl sc = (none>>) $ (do
    try $ string "open" >> spaces1
    n <- scopeName
    none
    let
      d = do
        char '{'
        lf
        ds <- manyTill (decl $ plus n sc) $ try $ none >> char '}'
        lf
        return $ Scope True n ds
      e = do
        lf
        return $ Open n
    choice [d,e]
  ) <|> (do
    n <- try $ do
      n' <- declName
      noneWrap $ char '='
      return n'
    e <- expr
    lf
    return $ Decl n sc e
  ) <|> (do
    n <- name
    none
    char '{'
    lf
    ds <- manyTill (decl $ plus n sc) $ try $ none >> char '}'
    lf
    return $ Scope False n ds
  )

expr :: Parser Expr
expr = (none>>) $ (do
    char '{'
    none
    lf
    ds <- manyTill (decl Nothing) $ try $ none >> char '}'
    none
    e <- term
    return $ Expr ds e
  ) <|> (do
    t <- term
    return $ Expr [] t
  )

term :: Parser Term
term = do
  none
  let
    s = do
      x <- eTerm
      none
      xs <- many $ do
        t <- eTerm
        none
        return t
      return $ x:xs
  ts <- s
  return $ foldl1 Apply ts

eTerm :: Parser Term
eTerm = (do
    string "\\" <|> string "\206\187" <|> string "\955"
    ss <- noneWrap args
    char '.'
    t <- noneWrap expr
    return $ Abst ss t
  ) <|> (do
    char '('
    t <- noneWrap expr
    char ')'
    return $ Wrap t
  ) <|> (do
    notFollowedBy $ string "open"
    v <- var
    return $ Var v
  )

signs :: Parser Char
signs = choice $ map char "-\'"

signName :: Parser ByteString
signName = choice $ map (fmap singleton . char) "[,]`"

declName :: Parser ByteString
declName = signName <|> do
  h <- alphaNum <|> signs
  hs <- many $ alphaNum <|> signs
  return $ pack $ h:hs

name :: Parser ByteString
name = do
  h <- upper <|> digit <|> char '-'
  hs <- many $ alphaNum <|> char '-'
  return $ pack $ h:hs

scopeName :: Parser ByteString
scopeName = do
  x <- name
  let
    si = do
      char '.'
      x <- name
      try ((x:) <$> si) <|> return [x]
  xs <- try si <|> return []
  return $ intercalate "." $ x:xs

var :: Parser ByteString
var = try (do
  s <- scopeName
  try (do
      char '.'
      n <- declName
      return $ concat [s,".",n]
    ) <|> return s
  ) <|> declName

args :: Parser [ByteString]
args = many1 $ (do
    s <- name
    x <- lookAhead anyChar
    if x == '.'
      then return ()
      else spaces1
    return s
  ) <|> (do
    fmap singleton lower
  )

