{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE LambdaCase #-}

module Utils.Actions
    ( getLine
    , printLine
    , interpret
    , TerminalM
    ) where

import Prelude (String, Functor, return, IO, ($), (<$), (<$>), (.))
import Control.Monad
import qualified System.IO as I
import Utils.Free

data Terminal a
  = GetLine (String -> a)
  | PrintLine String a
  deriving Functor

type TerminalM = Free Terminal

getLine :: TerminalM String
getLine = Free (GetLine return)

printLine :: String -> TerminalM ()
printLine str = liftF (PrintLine str ())


foldFree :: Monad m => (forall x . f x -> m x) -> Free f a -> m a
foldFree _ (Pure a)  = return a
foldFree f (Free as) = f as >>= foldFree f


interpret :: TerminalM a -> IO a
interpret = foldFree $ \case
  GetLine next ->
    next <$> I.getLine
  PrintLine str next ->
    next <$ I.putStrLn str
