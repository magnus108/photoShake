{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DeriveFunctor #-}

module Utils.Actions
    ( interpret
    , writeFile
    , readFile
    , getLine
    , TerminalM
    ) where

import Prelude (String, Functor, return, IO, ($), (<$), (<$>), (.))
import Control.Monad
import qualified System.IO as I
import Utils.Free

data Terminal a
  = WriteFile I.FilePath String a
  | ReadFile I.FilePath (String -> a)
  | GetLine (String -> a)
  deriving Functor

type TerminalM = Free Terminal

writeFile :: I.FilePath -> String -> TerminalM ()
writeFile fp str = liftF (WriteFile fp str ())

readFile :: I.FilePath -> TerminalM String
readFile fp = Free (ReadFile fp return)

getLine :: TerminalM String
getLine = Free (GetLine return)


interpret :: TerminalM a -> IO a
interpret = foldFree $ \case
  WriteFile fp str next ->
    next <$ I.writeFile fp str
  ReadFile fp next ->
    next <$> I.readFile fp
  GetLine next ->
    next <$> I.getLine
