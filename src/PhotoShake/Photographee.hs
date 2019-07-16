{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
module PhotoShake.Photographee
    ( Photographee(..)
    , findPhotographee
    , insertPhotographee
    , Grades(..)
    , myOptionsDecode 
    ) where


import Prelude hiding ((++))

import GHC.Generics (Generic)
import Data.Csv
import Data.Vector (Vector, find, (++), fromList, toList)
import Data.Char
import qualified Data.ByteString.Lazy as BL
import Data.ByteString.Lazy.UTF8 (fromString)

import System.FilePath

import PhotoShake.ShakeError
import Control.Exception

import Utils.ListZipper hiding (toList)

import Data.Aeson.TH as DA (deriveJSON, defaultOptions)
--delete me
catchAny :: IO a -> (SomeException -> IO a) -> IO a
catchAny = catch


type FullName = String
type Ident = String
type Grade = String

data Grades = Grades (ListZipper Grade)
            | NoGrades 

deriveJSON DA.defaultOptions ''Grades


data Photographee = Photographee 
    { _tea :: String 
    , _grade :: Grade
    , _name :: FullName
    , _ident :: Ident 
    } deriving (Generic, Show, Eq)

deriveJSON DA.defaultOptions ''Photographee

instance FromRecord Photographee
instance ToRecord Photographee




myOptionsDecode :: DecodeOptions
myOptionsDecode = defaultDecodeOptions { decDelimiter = fromIntegral (ord ';') }

myOptionsEncode :: EncodeOptions
myOptionsEncode = defaultEncodeOptions { encDelimiter = fromIntegral (ord ';') }


insertPhotographee :: FilePath -> Ident -> String -> FullName -> IO ()
insertPhotographee location photographeeId grade name = do
    -- badness
    let ext = takeExtension location
    _ <- case ext of
            ".csv" -> return ()
            _ -> throw BadCsv

    locationData' <- readFile location `catchAny` (\_ -> throw LocationConfigFileMissing)
    seq (length locationData') (return ())

    let locationData = decodeWith myOptionsDecode NoHeader $ fromString locationData' :: Either String (Vector Photographee)

    let studentData = case locationData of
            Left _ -> throw ParseLocationFile
            Right locData -> locData ++ (fromList [Photographee "xxx" grade name photographeeId])

    let moreData = encodeWith myOptionsEncode $ toList studentData --can throw error

    BL.writeFile location moreData




findPhotographee :: FilePath -> Ident -> IO Photographee
findPhotographee location photographeeId = do
    -- badness
    let ext = takeExtension location

    _ <- case ext of
            ".csv" -> return ()
            _ -> throw BadCsv

    locationData' <- readFile location `catchAny` (\_ -> throw ReadLocationFile)

    let locationData = decodeWith myOptionsDecode NoHeader $ fromString locationData'
    --could use some case of here and error handling
    let studentData = case locationData of
            Left _ -> throw ParseLocationFile
            Right locData -> find ((photographeeId ==) . _ident ) locData

    let student = case studentData of
            Nothing -> throw FindPhotographee
            Just x -> x 

    return student 
