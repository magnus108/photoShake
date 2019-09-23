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
    ) where

import Prelude (fromIntegral)
import qualified PhotoShake.Location as Location
import qualified PhotoShake.Id as Id
import Data.Char
import Control.Monad 
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


data Photographee 
    = NoPhotographee
    | YesPhotographee 
        { _tea :: String 
        , _grade :: String
        , _name :: String
        , _ident :: String 
        } 
        deriving (Show, Eq, Generic, ToJSON, FromJSON)

instance FromRecord Photographee
instance ToRecord Photographee


noPhotographee :: Photographee
noPhotographee = NoPhotographee


yesPhotographee :: String -> String -> String -> String -> Photographee
yesPhotographee = YesPhotographee 


photographee :: a -> (String -> String -> String -> String -> a) -> Photographee -> a
photographee f g = \case
    NoPhotographee -> f
    (YesPhotographee tea grade name ident) -> g tea grade name ident
    
myOptionsDecode :: DecodeOptions
myOptionsDecode = defaultDecodeOptions { decDelimiter = fromIntegral (ord ';') }


findPhotographee :: Location.Location -> Id.Id -> IO Photographee
findPhotographee location id = do
    Location.location (return noPhotographee) (\l -> do 
        locationData' <- BL.readFile l
        let locationData = decodeWith myOptionsDecode NoHeader $ locationData'
        --could use some case of here and error handling
        let studentData = case locationData of
                Left _ -> throw ParseLocationFile
                Right locData -> Id.id noPhotographee 
                        (\i -> fromMaybe noPhotographee (find ((i ==) . _ident ) locData)) id

        return studentData
        ) location
