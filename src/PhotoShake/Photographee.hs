{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
module PhotoShake.Photographee
    ( Photographee(..)
    , insertPhotographee
    , Grades(..)
    , parseGrades
    , findPhotographee2
    , findPhotographee3
    , myOptionsDecode 
    ) where


import Data.List (nub)

import Prelude hiding ((++), readFile, filter)
import qualified Prelude as PP ((++))

import GHC.Generics (Generic)
import Data.Csv
import Data.Vector (Vector, find, (++), fromList, toList, filter)
import Data.Char
import qualified Data.ByteString.Lazy as BL

import System.FilePath

import PhotoShake.Photographee2

import PhotoShake.ShakeError
import PhotoShake.Grade
import Control.Exception

import Utils.ListZipper hiding (toList)

import Data.Aeson.TH as DA (deriveJSON, defaultOptions)
--delete me
catchAny :: IO a -> (SomeException -> IO a) -> IO a
catchAny = catch


type FullName = String
type Ident = String





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

    locationData' <- BL.readFile location `catchAny` (\_ -> throw LocationConfigFileMissing)

    putStrLn $ show locationData'

    let locationData = decodeWith myOptionsDecode NoHeader $ locationData' :: Either String (Vector Photographee)

    let studentData = case locationData of
            Left _ -> throw ParseLocationFile
            Right locData -> locData ++ (fromList [photographee ("SYS_" PP.++ photographeeId) grade name "missing"])

    let moreData = encodeWith myOptionsEncode $ toList studentData --can throw error

    BL.writeFile location moreData



parseGrades :: FilePath -> IO Grades
parseGrades location = do
    -- badness
    let ext = takeExtension location
    _ <- case ext of
            ".csv" -> return ()
            _ -> throw BadCsv

    locationData' <-  BL.readFile location `catchAny` (\_ -> throw LocationConfigFileMissing)

    putStrLn $ show locationData'

    let locationData = decodeWith myOptionsDecode NoHeader $ locationData' :: Either String (Vector Photographee)

    let studentData = case locationData of
            Left _ -> throw ParseLocationFile
            Right locData -> locData

    let grades = nub $ toList $ fmap _grade studentData

    case grades of 
        [] -> return noGrades
        x:xs -> return $ yesGrades $ ListZipper [] x xs




    








findPhotographee :: FilePath -> Ident -> IO Photographee
findPhotographee location photographeeId = do
    -- badness

    putStrLn photographeeId
    let ext = takeExtension location

    _ <- case ext of
            ".csv" -> return ()
            _ -> throw BadCsv

    locationData' <- BL.readFile location `catchAny` (\_ -> throw ReadLocationFile)

    putStrLn $ show locationData'

    let locationData = decodeWith myOptionsDecode NoHeader $ locationData'
    --could use some case of here and error handling
    let studentData = case locationData of
            Left _ -> throw ParseLocationFile
            Right locData -> find ((photographeeId ==) . _ident ) locData

    let student = case studentData of
            Nothing -> throw FindPhotographee
            Just x -> x 

    return student 


findPhotographee2 :: FilePath -> Ident -> IO Photographee
findPhotographee2 location photographeeId = do
    -- badness

    putStrLn photographeeId
    let ext = takeExtension location

    _ <- case ext of
            ".csv" -> return ()
            _ -> throw BadCsv

    locationData' <- BL.readFile location `catchAny` (\_ -> throw ReadLocationFile)

    putStrLn $ show locationData'

    let locationData = decodeWith myOptionsDecode NoHeader $ locationData'
    --could use some case of here and error handling
    let studentData = case locationData of
            Left _ -> throw ParseLocationFile
            Right locData -> find ((photographeeId ==) . _tea ) locData

    let student = case studentData of
            Nothing -> throw FindPhotographee
            Just x -> x 

    return student 


findPhotographee3 :: FilePath -> Ident -> IO (Maybe Photographee)
findPhotographee3 location photographeeId = do
    -- badness

    let ext = takeExtension location

    _ <- case ext of
            ".csv" -> return ()
            _ -> throw BadCsv

    locationData' <- BL.readFile location `catchAny` (\_ -> throw ReadLocationFile)

    putStrLn $ show locationData'

    let locationData = decodeWith myOptionsDecode NoHeader $ locationData'
    --could use some case of here and error handling
    let studentData = case locationData of
            Left _ -> throw ParseLocationFile
            Right locData -> find ((photographeeId ==) . _ident ) locData

    return studentData
