{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NoImplicitPrelude #-}                                                                                                                                                                              
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE LambdaCase #-}                                                      

module PhotoShake.Dump
    ( dump
    , noDump
    , dumpCase
    , Dump
    , getDumpFiles
    ) where

import Data.Eq
import Text.Show
import GHC.Generics
import System.FilePath
import Data.Aeson
import System.IO





import Data.Function (($), id)
import Data.Bool
import Data.Functor
import Control.Monad
import PhotoShake.ShakeError
import Control.Exception
import Data.List
import System.Directory


data Dump 
    = Dump FilePath
    | NoDump
    deriving (Show, Eq, Generic, ToJSON, FromJSON)


dump :: FilePath -> Dump
dump = Dump


noDump :: Dump
noDump = NoDump



dumpCase :: a -> (FilePath -> a) -> Dump -> a
dumpCase f g = \case
    NoDump -> f
    Dump x -> g x







-- concider moving this or getting rid of it
catchAny :: IO a -> (SomeException -> IO a) -> IO a
catchAny = catch



getDumpFiles :: Dump -> IO [(FilePath, FilePath)]
getDumpFiles dump = do
    case dump of 
        NoDump -> return []
        Dump x -> do
            files <- listDirectory x `catchAny` (\_ -> throw DumpMissing)
            let files' = filter (\z -> isExtensionOf "CR2" z || (isExtensionOf "cr2" z)) files -- bad use
            let files2' = filter (\z -> isExtensionOf "JPG" z || (isExtensionOf "jpg" z)) files -- bad use
            files'' <- mapM (\file -> do 
                    b <- (doesFileExist (x </> file -<.> "JPG")) 
                    b1 <- (doesFileExist (x </> file -<.> "jpg")) 
                    let b3 = all id $ fmap (\z -> elem (z -<.> "CR2") files' || (elem (z -<.> "cr2") files')) files2'
                    if ( b || b1 ) && b3 then return file else throw JPGMissing ) files' -- this is error
            return $ fmap (\y -> (x </> y, x </> y -<.> "JPG")) files'' -- could be nicer

