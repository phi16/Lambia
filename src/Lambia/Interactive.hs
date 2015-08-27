{-# LANGUAGE OverloadedStrings #-}

module Lambia.Interactive (interactive) where

import Prelude hiding (getLine, putStr, putStrLn, null)
import Data.ByteString.Char8 hiding (append)
import qualified Data.ByteString.Char8 as B
import Control.Applicative hiding (empty)
import Control.Monad
import Control.Monad.State.CPS
import Control.Monad.Trans.Class
import Control.Monad.Trans.Except

import Lambia.Parse
import Lambia.Index
import Lambia.Apply hiding (lift)
import Lambia.Types

type Act a = StateT Status IO a

interactive :: (Save,Save) -> IO ()
interactive (s1,s2) = void $ runStateT (ev empty) $ Status s1 s2

ev :: ByteString -> Act ()
ev str = do
  lift $ putStr $ if null str then "> " else "| "
  x <- flip snoc '\n' <$> lift getLine
  let ss = str`B.append`x
  case parseLines ss of
    Left err -> case "unexpected \'\\NUL\'"`isInfixOf`err of
      True -> do
        ev ss
      False -> do
        lift $ putStrLn err
        ev ""
    Right e -> do
      s <- get
      let
        (r,s') = flip runState s $ runExceptT $ case e of
          Left d -> Left <$> ixDecl d
          Right x -> Right <$> ixExpr x
      s'' <- case r of
        Left f -> do
          lift $ putStrLn f
          return s'
        Right (Left d') -> return s'
        Right (Right e') -> do
          let v = fst $ apply e'
          lift $ putStrLn $ pack $ show v
          return $ case s' of
            Status a u -> Status a $ append "it" Nothing v u
      put s''
      ev ""

