{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DeriveFunctor #-}

module Utils.Actions
    ( interpret
    , writeFile
    , readFile
    , TerminalM
    ) where

import Prelude (String, Functor, return, IO, ($), (<$), (<$>), (.))
import Control.Monad
import qualified Data.ByteString.Lazy as BL

import Utils.Free
import System.FilePath

data Terminal a
  = WriteFile FilePath BL.ByteString a
  | ReadFile FilePath (BL.ByteString -> a)
  deriving Functor

type TerminalM = Free Terminal

writeFile :: FilePath -> BL.ByteString -> TerminalM ()
writeFile fp str = liftF (WriteFile fp str ())

readFile :: FilePath -> TerminalM BL.ByteString
readFile fp = Free (ReadFile fp return)

interpret :: TerminalM a -> IO a
interpret = foldFree $ \case
  WriteFile fp str next ->
    next <$ BL.writeFile fp str
  ReadFile fp next ->
    next <$> BL.readFile fp
