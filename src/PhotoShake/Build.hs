{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NoImplicitPrelude #-}                                                                                                                                                                              
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE LambdaCase #-}                                                      
module PhotoShake.Build
    ( Build
    , doneBuild
    , building
    , noFind
    , noBuild
    , build'
    ) where

import PhotoShake.Photographee2
import Data.String 
import Data.Eq
import Text.Show
import GHC.Generics
import Data.Aeson


data Build
    = DoneBuild Photographee String
    | Building Photographee String
    | NoFind String
    | NoBuild
    deriving (Show, Eq, Generic, ToJSON, FromJSON)

noFind :: String -> Build
noFind = NoFind

noBuild :: Build
noBuild = NoBuild

building :: Photographee -> String -> Build
building = Building

doneBuild :: Photographee -> String -> Build
doneBuild = DoneBuild

build' :: a -> (String -> a) -> Build -> a
build' f g = \case
    DoneBuild _ s -> g s
    Building _ s -> g s
    NoFind s -> g s
    NoBuild -> f