module Main where

import Prelude hiding (getContents)
import Control.Applicative
import Data.ByteString (pack, unpack, getContents)
import Data.Attoparsec.ByteString

import Lambia.Parse
import Lambia.Index

main :: IO ()
main = do
  str <- getContents
  print $ do
    u <- parseSource str
    indexing u

