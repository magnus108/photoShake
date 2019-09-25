{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NoImplicitPrelude #-}                                                                                                                                                                              
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE LambdaCase #-}                                                      

module PhotoShake.Photographee2
    ( Photographee
    , findPhotographee
    , insert
    , fromGrade
    , photographee
    , _name
    , _ident
    , _tea
    , _grade
    ) where

import qualified Utils.ListZipper as ListZipper
import qualified Data.Vector as Vector
import Prelude (fromIntegral, seq)
import qualified PhotoShake.Location as Location
import qualified PhotoShake.Id as Id
import Data.Int
import Data.Char
import Control.Monad 
import Data.Monoid
import Data.Maybe
import Data.Either
import System.IO 
import Data.String
import Data.Eq
import Text.Show
import GHC.Generics
import Data.Aeson
import qualified Data.ByteString.Lazy as BL
import Data.Csv
import PhotoShake.ShakeError
import qualified Data.List as List
import Data.Function ((.), ($))
import qualified PhotoShake.Grade as Grade

import System.Random


data Photographee = Photographee 
    { _tea :: String 
    , _grade :: String
    , _name :: String
    , _ident :: String 
    } deriving (Show, Eq, Generic, ToJSON, FromJSON)

instance FromRecord Photographee
instance ToRecord Photographee
 

photographee :: String -> String -> String -> String -> Photographee
photographee = Photographee


myOptionsDecode :: DecodeOptions
myOptionsDecode = defaultDecodeOptions { decDelimiter = fromIntegral (ord ';') }

myOptionsEncode :: EncodeOptions
myOptionsEncode = defaultEncodeOptions { encDelimiter = fromIntegral (ord ';') }


findPhotographee :: Location.Location -> Id.Id -> IO (Maybe Photographee)
findPhotographee location id = do
    Location.location (return Nothing) (\l -> do 
        locationData' <- BL.readFile l
        let locationData = decodeWith myOptionsDecode NoHeader $ locationData'
        --could use some case of here and error handling
        let studentData = case locationData of
                Left _ -> throw ParseLocationFile
                Right locData -> Id.id Nothing 
                        (\i -> List.find ((i ==) . _ident ) locData) id
        return studentData
        ) location


insert :: Location.Location -> Grade.Grade -> String -> String -> IO (Maybe ())
insert location grade id name = do
    Location.location (return Nothing) (\l -> do 
        locationData' <- BL.readFile l
        seq (BL.length locationData') (return ())
        let locationData = decodeWith myOptionsDecode NoHeader $ locationData' :: Either String (Vector.Vector Photographee)

        let low = 1000000 :: Int
        let high = 9999999 :: Int
        r <- getStdRandom (randomR (low, high))
        let studentData = case locationData of
                Left _ -> throw ParseLocationFile
                Right locData -> locData Vector.++ (Vector.fromList [photographee ("SYS_" List.++ id) grade name ("ny_" List.++ (show r))])

        let moreData = encodeWith myOptionsEncode $ Vector.toList studentData --can throw error
        BL.writeFile l moreData
        return $ Just ()
        ) location


fromGrade :: Location.Location -> Grade.Grades -> IO [Photographee]
fromGrade location grades =
    Grade.grades (return []) (\g -> do
        Location.location (return []) (\l -> do -- kind of bad
            locationData' <-  BL.readFile l 
            let locationData = decodeWith myOptionsDecode NoHeader $ locationData' 
            let studentData = case locationData of
                    Left _ -> throw ParseLocationFile
                    Right locData -> Vector.filter (((ListZipper.focus g) ==) . _grade) locData
            return $ Vector.toList $ studentData
            ) location) grades
