module Main where

import Prelude hiding (getContents)
import Control.Applicative
import Data.ByteString.Char8 (pack, unpack, getContents)
import qualified Data.ByteString.Char8 as B

import Lambia.Parse
import Lambia.Index
import Lambia.Apply

main :: IO ()
main = do
  str <- getContents
  let
    e = do
      u <- parseSource str
      indexing u
  case e of
    Left err -> B.putStrLn err
    Right t -> case t of
      (Just l,_) -> putStrLn $ unlines $ map show $ apply l
      _ -> print t
