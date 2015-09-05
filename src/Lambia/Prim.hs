{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Lambia.Prim (prim) where

import Control.Applicative hiding (empty)
import Data.Map (empty)
import qualified Data.ByteString.Char8 as B
import Data.ByteString.Char8 (ByteString)
import Data.Attoparsec.ByteString.Char8

import Lambia.Types

nil :: Save s
nil = Save (empty, const Nothing)

prim :: Store s => Save s
prim = Save (empty, makePrim (foldPrim prs) B.empty)

newtype Pri s = Pri (ByteString -> Maybe (Pri s, Maybe (Syn s)))

pnil :: Pri s
pnil = Pri $ const Nothing

add :: ByteString -> ByteString -> ByteString
add x y = if B.null x then y else x`B.append`B.cons '.' y

makePrim :: Store s => Pri s -> ByteString -> Primitive s
makePrim (Pri x) p = \s -> let ss = add p s in case x s of
  Just (p',j) -> Just (Save (empty, makePrim p' ss), fmap ((,Just $ add p ss) . fromSyn) j)
  Nothing -> Nothing

foldPrim :: [([ByteString], Pri s)] -> Pri s
foldPrim = foldl meld pnil where
  merge :: Pri s -> Pri s -> Pri s
  merge (Pri f) (Pri p) = Pri $ \s -> case f s of
    Nothing -> p s
    Just (pr,m) -> case p s of
      Nothing -> Just (pr,m)
      Just (pl,n) -> Just (merge pr pl, m <|> n)
  meld :: Pri s -> ([ByteString], Pri s) -> Pri s
  meld f ([], p) = merge f p
  meld (Pri f) (x:xs, p) = Pri $ \s -> case f s of
    Nothing -> if s == x then Just (meld pnil (xs,p), Nothing) else Nothing
    Just (pr,m) -> if s == x then Just (meld pr (xs,p),m) else Just (pr,m)

prs :: [([ByteString], Pri s)]
prs = [
    (["Primitive","Nat"], genNats),
    (["Primitive","Combinator"], genCombi),
    (["Primitive","Int","I"], pnil),
    (["Primitive","Integer"], pnil),
    (["Primitive","Char"], pnil),
    (["Primitive","Decimal"], pnil),
    (["Primitive","IO"], pnil)
  ]

iterateN :: Integer -> (a -> a) -> a -> a
iterateN 0 f = id
iterateN n f = let g = iterateN (n`div`2) f in case even n of
  True -> g . g
  False -> g . g . f

runParser :: Parser a -> ByteString -> Maybe a
runParser p s = case parseOnly (p <* endOfInput) s of
  Left _ -> Nothing
  Right x -> Just x

exact :: (ByteString -> Maybe a) -> (a -> Syn s) -> Pri s
exact f g = Pri $ \s -> case f s of
  Nothing -> Nothing
  Just x -> Just (pnil, Just $ g x)

genNats :: Pri s
genNats = runParser decimal `exact` \d -> 
  Lm $ Lm $ iterateN d (Ap (Ix 1)) (Ix 0)

genCombi :: Pri s
genCombi = let
    s = Lm $ Lm $ Lm $ Ap (Ap (Ix 2) $ Ix 0) $ Ap (Ix 1) $ Ix 0
    k = Lm $ Lm $ Ix 1
    i = Lm $ Ix 0
    c = Lm $ Lm $ Lm $ Ap (Ap (Ix 2) $ Ix 0) $ Ix 1
    b = Lm $ Lm $ Lm $ Ap (Ix 2) $ Ap (Ix 1) $ Ix 0
    ps = many' $ satisfy $ inClass "SKICB"
    toC 'S' = s
    toC 'K' = k
    toC 'I' = i
    toC 'C' = c
    toC 'B' = b
  in runParser ps `exact` \ds -> foldl1 Ap $ map toC ds
