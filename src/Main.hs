module Main where

import Prelude hiding (getContents, readFile)
import Control.Applicative
import Control.Monad
import Data.List (partition)
import Data.ByteString.Char8 (pack, unpack, getContents, readFile)
import qualified Data.ByteString.Char8 as B
import System.Environment

import Lambia.Parse
import Lambia.Index
import Lambia.Apply
import Lambia.Interactive

main :: IO ()
main = do
  args <- getArgs
  let (opt,fn) = partition ((=='-') . head) args
  if null fn
    then interactive (nil,nil)
    else do
      str <- readFile $ head fn
      let
        e = do
          u <- parseSource str
          indexing u
      case e of
        Left err -> B.putStrLn err
        Right t -> case t of
          (Just l,v) -> do
            let b = "-i"`elem`opt
            when b $ putStrLn "> [Source]"
            print $ fst $ apply l
            when b $ interactive v
          (Nothing,v) -> interactive v
