{-# LANGUAGE DatatypeContexts #-} --https://stackoverflow.com/questions/22622399/how-to-fix-illegal-datatype-context-use-xdatatypecontexts/22622591#22622591
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DeriveFunctor #-}

module Utils.Actions
    ( interpret
    , writeFile
    , readFile
    , TerminalM
    ) where

import Prelude (String, Functor, return, IO, ($), (<$), (.))
import Data.Functor ((<&>))
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString as B

import Utils.Free
import System.FilePath

import Prelude (error)
import Conduit
import Data.Conduit.Combinators
import Control.Monad
import Data.Conduit.Attoparsec
import Data.Aeson (FromJSON, fromJSON, ToJSON, toJSON, encode, json, Value, Result(Error, Success), toEncoding, fromEncoding)

data (FromJSON x, ToJSON x) => Terminal x a
  = WriteFile FilePath x a
  | ReadFile FilePath (x -> a)
  deriving Functor

type TerminalM x = Free (Terminal x) 


sinkFromJSON :: (MonadThrow m, FromJSON a) => ConduitM ByteString o m a
sinkFromJSON = do
    value <- sinkParser json
    case fromJSON value of
        Error e -> error e
        Success x -> return x

writeFile :: (ToJSON x, FromJSON x) => FilePath -> x -> TerminalM x ()
writeFile fp str = liftF (WriteFile fp str ())

readFile :: (FromJSON x, ToJSON x) => FilePath -> TerminalM x x
readFile fp = Free (ReadFile fp return)


interpret :: (FromJSON x, ToJSON x) => TerminalM x a -> IO a
interpret = foldFree $ \case
  WriteFile fp j next ->
    next <$ x
        where
            x = runConduitRes $ yield (fromEncoding $ toEncoding j)
                             .| builderToByteString
                             .| sinkFileBS fp
  ReadFile fp next ->
    fmap next x
        where
            x = runConduitRes $ sourceFileBS fp
                             .| sinkFromJSON 
