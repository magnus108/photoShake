{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
module PhotoShake.Photographee
    ( Photographee(..)
    , findPhotographee
    , insertPhotographee
    , Idd(..)
    , Grades(..)
    , GradeSelection(..)
    , parseGrades
    , findPhotographee2
    , findPhotographee3
    , myOptionsDecode 
    , parsePhotographees 
    ) where


import Data.List (nub)

import Prelude hiding ((++), readFile, filter)
import qualified Prelude as PP ((++))

import GHC.Generics (Generic)
import Data.Csv
import Data.Vector (Vector, find, (++), fromList, toList, filter)
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

data GradeSelection = GradeSelection String
                    | NoSelection
                    deriving (Show, Eq)


data Idd = Idd String deriving (Show, Eq)
deriveJSON DA.defaultOptions ''Idd

deriveJSON DA.defaultOptions ''GradeSelection

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

    locationData' <- BL.readFile location `catchAny` (\_ -> throw LocationConfigFileMissing)

    putStrLn $ show locationData'

    let locationData = decodeWith myOptionsDecode NoHeader $ locationData' :: Either String (Vector Photographee)

    let studentData = case locationData of
            Left _ -> throw ParseLocationFile
            Right locData -> locData ++ (fromList [Photographee ("SYS_" PP.++ photographeeId) grade name "missing"])

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
        [] -> return NoGrades
        x:xs -> return $ Grades $ ListZipper [] x xs



parsePhotographees :: FilePath -> GradeSelection -> IO [Photographee]
parsePhotographees location gradeSelection = do
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
            Right locData -> case gradeSelection of
                NoSelection -> mempty
                GradeSelection grade -> filter ((grade ==). _grade) locData

    return $ toList $ studentData

    








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
