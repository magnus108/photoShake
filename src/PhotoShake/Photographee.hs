{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
module PhotoShake.Photographee
    ( Photographee(..)
    , findPhotographee
    , insertPhotographee
    ) where

import GHC.Generics (Generic)
import Data.Csv
import Data.Vector (find)
import Data.Char
import Data.ByteString.Lazy.UTF8 (fromString)

import System.FilePath

import PhotoShake.ShakeError
import Control.Exception

import Data.Aeson.TH as DA (deriveJSON, defaultOptions)
--delete me
catchAny :: IO a -> (SomeException -> IO a) -> IO a
catchAny = catch

type FullName = String
type Ident = String


data Photographee = Photographee 
    { _tea :: String 
    , _grade :: String  -- this is not good
    , _name :: FullName
    , _ident :: Ident 
    } deriving (Generic, Show, Eq)

deriveJSON DA.defaultOptions ''Photographee

instance FromRecord Photographee
instance ToRecord Photographee




myOptions :: DecodeOptions
myOptions = defaultDecodeOptions { decDelimiter = fromIntegral (ord ';') }


insertPhotographee :: FilePath -> Ident -> String -> FullName -> IO ()
insertPhotographee location photographeeId grade name =
    return ()

findPhotographee :: FilePath -> Ident -> IO Photographee
findPhotographee location photographeeId = do
    -- badness
    let ext = takeExtension location

    _ <- case ext of
            ".csv" -> return ()
            _ -> throw BadCsv

    locationData' <- readFile location `catchAny` (\_ -> throw ReadLocationFile)

    let locationData = decodeWith myOptions NoHeader $ fromString locationData'
    --could use some case of here and error handling
    let studentData = case locationData of
            Left _ -> throw ParseLocationFile
            Right locData -> find ((photographeeId ==) . _ident ) locData

    let student = case studentData of
            Nothing -> throw FindPhotographee
            Just x -> x 

    return student 
