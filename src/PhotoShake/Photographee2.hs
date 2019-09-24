{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NoImplicitPrelude #-}                                                                                                                                                                              
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE LambdaCase #-}                                                      

module PhotoShake.Photographee2
    ( Photographee
    , noPhotographee
    , yesPhotographee
    , photographee
    , findPhotographee
    , fromGrade
    ) where

import qualified Utils.ListZipper as ListZipper
import qualified Data.Vector as Vector
import Prelude (fromIntegral)
import qualified PhotoShake.Location as Location
import qualified PhotoShake.Id as Id
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
import Data.List 
import Data.Function ((.), ($))
import qualified PhotoShake.Grade as Grade



data PhotographeeData = PhotographeeData
    { _tea :: String 
    , _grade :: String
    , _name :: String
    , _ident :: String 
    } deriving (Show, Eq, Generic, ToJSON, FromJSON)

instance FromRecord PhotographeeData
instance ToRecord PhotographeeData
instance FromField PhotographeeData
instance ToField PhotographeeData

data Photographee 
    = NoPhotographee
    | YesPhotographee PhotographeeData
        deriving (Show, Eq, Generic, ToJSON, FromJSON)

instance FromRecord Photographee
instance ToRecord Photographee


noPhotographee :: Photographee
noPhotographee = NoPhotographee


yesPhotographee :: PhotographeeData -> Photographee
yesPhotographee = YesPhotographee 


photographee :: a -> (PhotographeeData -> a) -> Photographee -> a
photographee f g = \case
    NoPhotographee -> f
    (YesPhotographee x) -> g x
    

myOptionsDecode :: DecodeOptions
myOptionsDecode = defaultDecodeOptions { decDelimiter = fromIntegral (ord ';') }



tryIdent :: Photographee -> Maybe String
tryIdent = photographee Nothing (Just . _ident) 

findPhotographee :: Location.Location -> Id.Id -> IO Photographee
findPhotographee location id = do
    Location.location (return noPhotographee) (\l -> do 
        locationData' <- BL.readFile l
        let locationData = decodeWith myOptionsDecode NoHeader $ locationData'
        --could use some case of here and error handling
        let studentData = case locationData of
                Left _ -> throw ParseLocationFile
                Right locData -> Id.id noPhotographee 
                        (\i -> fromMaybe noPhotographee (find ((Just i ==) . tryIdent ) locData)) id
        return studentData
        ) location


fromGrade :: Location.Location -> Grade.Grades -> IO [PhotographeeData]
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
