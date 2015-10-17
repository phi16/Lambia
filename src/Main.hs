{-# LANGUAGE OverloadedStrings #-}

module Main where

import Prelude hiding (getContents, readFile)
import Control.Monad
import Data.List (partition)
import Data.ByteString.Char8 (readFile)
import qualified Data.ByteString.Char8 as B
import System.Environment

import Lambia.Parse
import Lambia.Index
import Lambia.Types
import Lambia.Combi ()
import Lambia.Unlambda ()
import Lambia.Prim
import Lambia.Interactive

main :: IO ()
main = do
  args <- getArgs
  let
    (opt,fn) = partition ((=='-') . head) args
    mode = if "-u"`elem`opt
      then 2 -- Unlambda
      else if "-s"`elem`opt
        then 1 -- SKI
        else 0 -- Lambda
  if null fn
    then case mode of
      0 -> interactive (prim,prim :: Save Lambda)
      1 -> interactive (prim,prim :: Save Combi)
      2 -> interactive (prim,prim :: Save Unlambda)
    else do
      str <- readFile $ head fn
      case mode of
        0 -> run opt $ do
          u <- parseSource (head fn) str
          indexing u :: Indexed Lambda
        1 -> run opt $ do
          u <- parseSource (head fn) str
          indexing u :: Indexed Combi
        2 -> run opt $ do
          u <- parseSource (head fn) str
          indexing u :: Indexed Unlambda

run :: Store s => [String] -> Indexed s -> IO ()
run opt e = case e of
  Left err -> B.putStrLn err
  Right t -> case t of
    (Just l,v) -> do
      let b = "-i"`elem`opt
      when b $ putStrLn "> [Source]"
      let l' = fst $ apply l
      print l'
      let v' = (fst v, append "it" Nothing l' $ snd v)
      when b $ interactive v'
    (Nothing,v) -> interactive v
