{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Lambia.Interactive (interactive) where

import Prelude hiding (getLine, putStr, putStrLn, filter)
import Data.Char (isSpace)
import Data.ByteString.Char8 hiding (
  append, map, splitAt, init,
  dropWhile, head, null, isPrefixOf)
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
import Lambia.Apply ()
import Lambia.Types

type Act s a = InputT (StateT (Status s) IO) a

cf :: Store s => CompletionFunc (StateT (Status s) IO)
cf = completeWordWithPrev Nothing " ()`\'-,[]" $ \pv wd -> do
  Status _ (Save (l,_)) <- get
  let
    (pva,pvb) = splitAt 4 $ dropWhile (==' ') pv
    op = pva == "nepo" && (null pvb || head pvb == ' ')
    wds = map pack $ splitOn "." wd
    toComp :: M.Map ByteString (Entity s) -> [Completion]
    toComp m = flip map (M.toList m) $ \(k,(Save (s,_),v)) -> let
        k' = unpack k
      in case v of
        Nothing -> if op
          then Completion k' k' True
          else Completion (k'++".") k' False
        Just e -> Completion k' k' $ M.null s
    addComp :: ByteString -> Completion -> Completion
    addComp s (Completion a b c) = Completion (t a) (t b) c where
      t x = s' ++ "." ++ x
      s' = unpack s
    word :: [ByteString] -> Save s -> [Completion]
    word [] (Save (s,_)) = toComp s
    word [x] (Save (s,_)) = toComp $ M.filterWithKey (\k _ -> x`B.isPrefixOf`k) s
    word (x:xs) (Save (s,_)) = case M.lookup x s of
      Nothing -> []
      Just (y,_) -> map (addComp x) $ word xs y
  return $ word wds $ Save $ (,const Nothing) $ if op
    then M.filter (\(Save (x,_),_) -> not $ M.null x) l
    else l

setting :: Store s => Settings (StateT (Status s) IO)
setting = setComplete cf defaultSettings

interactive :: Store s => (Save s,Save s) -> IO ()
interactive (s1,s2) = let
    i = runInputT setting $ withInterrupt $ ev empty
  in void $ runStateT i $ Status s1 s2

ev :: Store s => ByteString -> Act s ()
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
        then outputStrLn "[Quit]"
        else if B.null str && B.take 3 x == ":s "
          then search $ B.dropWhile isSpace $ B.drop 3 x
          else proc $ str`B.append`x

search :: Store s => ByteString -> Act s ()
search ss = do
  Status _ l <- lift $ get
  let
    str = init $ unpack ss
    s = map pack $ splitOn "." str
    fu :: [ByteString] -> Save s -> Maybe s
    fu [] _ = Nothing
    fu [x] (Save (s,u)) = join $ (fmap fst . snd) <$> (M.lookup x s <|> u x)
    fu (x:xs) (Save (s,u)) = case M.lookup x s <|> u x of
      Just (s',_) -> fu xs s'
      Nothing -> Nothing
  case fu s l of
    Nothing -> outputStrLn $ "Not in scope : " ++ str
    Just l -> outputStrLn $ str ++ " = " ++ show l
  ev ""
proc :: Store s => ByteString -> Act s ()
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
          hdl :: MonadIO m => a -> SomeException -> InputT m a
          hdl a _ = outputStrLn "[Interrupt]" >> return a
        handle (hdl s') $ do
          outputStrLn $ show v
          return $ case s' of
            Status a u -> Status a $ append "it" Nothing v u
    lift $ put s''
    ev ""

