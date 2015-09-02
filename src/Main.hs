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
import Lambia.Interactive

main :: IO ()
main = do
  args <- getArgs
  let (opt,fn) = partition ((=='-') . head) args
  if null fn
    then if "-s"`elem`opt
      then interactive (nil,nil :: Save Combi)
      else interactive (nil,nil :: Save Lambda)
    else do
      str <- readFile $ head fn
      if "-s"`elem`opt
        then run opt $ do
          u <- parseSource (head fn) str
          indexing u :: Indexed Combi
        else run opt $ do
          u <- parseSource (head fn) str
          indexing u :: Indexed Lambda

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
