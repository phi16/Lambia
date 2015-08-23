module Main where

import Prelude hiding (getContents,putStrLn)
import Control.Applicative
import Data.ByteString.Char8 (pack, unpack, getContents, putStrLn)

import Lambia.Parse
import Lambia.Index

main :: IO ()
main = do
  str <- getContents
  let
    e = do
      u <- parseSource str
      indexing u
  case e of
    Left err -> putStrLn err
    Right t -> print t
