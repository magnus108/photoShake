--{-# LANGUAGE DatatypeContexts #-} --https://stackoverflow.com/questions/22622399/how-to-fix-illegal-datatype-context-use-xdatatypecontexts/22622591#22622591
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveFunctor #-}
{-# Language GADTs #-}


module Utils.Actions
    ( interpret
    , writeFile
    , readFile
    , TerminalM
    ) where

import Prelude (Functor, return, IO, ($), (<$))
import Data.ByteString (ByteString)

import Utils.Free
import Utils.FP

import Prelude (error)
import Conduit
import Control.Monad
import Data.Conduit.Attoparsec
import Data.Aeson (FromJSON, fromJSON, ToJSON, json, Result(Error, Success), toEncoding, fromEncoding)




data Terminal x a where
  WriteFile :: (FromJSON x, ToJSON x) => FP -> x -> a -> Terminal x a
  ReadFile :: (FromJSON x, ToJSON x) => FP -> (x -> a) -> Terminal x a

deriving instance Functor (Terminal x)

type TerminalM x = Free (Terminal x)


sinkFromJSON :: (MonadThrow m, FromJSON a) => ConduitM ByteString o m a
sinkFromJSON = do
    value <- sinkParser json
    case fromJSON value of
        Error e -> error e
        Success x -> return x


writeFile :: (ToJSON x, FromJSON x) => FP -> x -> TerminalM x ()
writeFile fp str = liftF (WriteFile fp str ())


readFile :: (FromJSON x, ToJSON x) => FP -> TerminalM x x
readFile fp = Free (ReadFile fp return)


interpret :: (FromJSON x, ToJSON x) => TerminalM x a -> IO a
interpret = foldFree $ \case
  WriteFile fp j next ->
    next <$ x
        where
            x = runConduitRes $ yield (fromEncoding $ toEncoding j)
                             .| builderToByteString
                             .| sinkFileBS (toFilePath fp)
  ReadFile fp next ->
    fmap next x
        where
            x = runConduitRes $ sourceFileBS (toFilePath fp)
                             .| sinkFromJSON 
