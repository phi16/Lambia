{-# LANGUAGE OverloadedStrings #-}

module Lambia.Interactive (interactive) where

import Prelude hiding (getLine, putStr, putStrLn, filter)
import Data.Char (isSpace)
import Data.ByteString.Char8 hiding (
  append, map, splitAt,
  dropWhile, head, null, isPrefixOf)
import Data.Maybe
import Data.List (isPrefixOf)
import Data.List.Split (splitOn)
import qualified Data.Map as M
import qualified Data.ByteString.Char8 as B
import Control.Applicative hiding (empty)
import Control.Monad
import Control.Monad.State.Strict
import Control.Monad.Trans.Except
import System.Console.Haskeline

import Lambia.Parse
import Lambia.Index
import Lambia.Apply hiding (lift)
import Lambia.Types

type Act a = InputT (StateT Status IO) a

cf :: CompletionFunc (StateT Status IO)
cf = completeWordWithPrev Nothing " ()`\'-,[]" $ \pv wd -> do
  Status _ (Save l) <- get
  let
    (pva,pvb) = splitAt 4 $ dropWhile (==' ') pv
    op = pva == "nepo" && (null pvb || head pvb == ' ')
    wds = map pack $ splitOn "." wd
    toComp :: M.Map ByteString Entity -> [Completion]
    toComp m = flip map (M.toList m) $ \(k,(Save s,v)) -> let
        k' = unpack k
      in case v of
        Nothing -> Completion (k'++".") k' False
        Just e -> if M.null s
          then Completion k' k' True
          else Completion k' k' False
    addComp :: ByteString -> Completion -> Completion
    addComp s (Completion a b c) = Completion (t a) (t b) c where
      t x = s' ++ "." ++ x
      s' = unpack s
    word :: [ByteString] -> Save -> [Completion]
    word [] (Save s) = toComp s
    word [x] (Save s) = toComp $ M.filterWithKey (\k _ -> x`B.isPrefixOf`k) s
    word (x:xs) (Save s) = case M.lookup x s of
      Nothing -> []
      Just (y,_) -> map (addComp x) $ word xs y
  return $ word wds $ Save $ if op
    then M.filter (\(Save x,_) -> not $ M.null x) l
    else l

setting :: Settings (StateT Status IO)
setting = setComplete cf defaultSettings

interactive :: (Save,Save) -> IO ()
interactive (s1,s2) = let
    i = runInputT setting $ withInterrupt $ ev empty
  in void $ runStateT i $ Status s1 s2

ev :: ByteString -> Act ()
ev str = do
  let hdl :: MonadIO m => SomeException -> m (Maybe String)
      hdl _ = return Nothing
  xi <- handle hdl $ do
    getInputLine $ if B.null str then "> " else "| "
  let xj = fmap (flip snoc '\n' . pack) xi
  case xj of
    Nothing -> ev ""
    Just x -> if B.null $ filter (not . isSpace) x
      then ev str
      else if x == ":q\n"
        then return ()
        else proc $ str`B.append`x

proc :: ByteString -> Act ()
proc ss = case parseLines ss of
  Left err -> case "unexpected \'\\NUL\'"`isInfixOf`err of
    True -> do
      ev ss
    False -> do
      outputStrLn $ unpack err
      ev ""
  Right e -> do
    s <- lift $ get
    let
      (r,s') = flip runState s $ runExceptT $ case e of
        Left d -> Left <$> ixDecl d
        Right x -> Right <$> ixExpr x
    s'' <- case r of
      Left f -> do
        outputStrLn $ unpack f
        return s'
      Right (Left d') -> return s'
      Right (Right e') -> do
        let
          v = fst $ apply e'
          hdl :: MonadIO m => SomeException -> InputT m Status
          hdl _ = outputStrLn "[Interrupt]" >> return s'
        handle hdl $ do
          outputStrLn $ show v
          return $ case s' of
            Status a u -> Status a $ append "it" Nothing v u
    lift $ put s''
    ev ""

