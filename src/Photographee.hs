{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
module Photographee
    ( Photographee(..)
    , findPhotographee
    ) where

import GHC.Generics (Generic)
import Data.Csv
import Data.Vector (find)
import Data.ByteString.Lazy.UTF8 (fromString)
import Data.Maybe

import PhotoShake.ShakeConfig
import ShakeError

type FullName = String
type Ident = String


data Photographee = Photographee 
    { _name :: FullName
    , _grade :: String  -- this is not good
    , _ident :: Ident 
    } deriving (Generic, Show)


instance FromRecord Photographee


findPhotographee :: FilePath -> Ident -> IO Photographee
findPhotographee location photographeeId = do
    locationData' <- readFile location `catchAny` (\_ -> throw ReadLocationFile)
    let locationData = decode NoHeader $ fromString locationData'
    --could use some case of here and error handling
    studentData <- case locationData of
            Left _ -> throw ParseLocationFile
            Right locData -> return $ find ((photographeeId ==) . _ident ) locData

    student <- (return $ fromJust studentData) `catchAny` (\_ -> throw FindPhotographee)
    return student 
