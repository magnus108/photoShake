{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NoImplicitPrelude #-}                                                                                                                                                                              
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE LambdaCase #-}                                                      
module PhotoShake.Location
    ( Location
    , location
    , yesLocation
    , noLocation
    , setLocation
    , getLocation
    ) where

import Data.Eq
import Data.String
import Text.Show
import GHC.Generics
import System.FilePath
import Data.Aeson
import Utils.Actions
import Utils.FP
import Utils.ListZipper

data Location 
    = YesLocation FilePath
    | NoLocation
    deriving (Show, Eq, Generic, ToJSON, FromJSON)


yesLocation :: FilePath -> Location
yesLocation = YesLocation


noLocation :: Location
noLocation = NoLocation


location :: a -> (FilePath -> a) -> Location -> a
location f g = \case
    NoLocation -> f
    YesLocation x -> g x


getLocation :: FP -> TerminalM Location Location
getLocation = readFile 


setLocation :: FP -> Location -> TerminalM Location ()
setLocation = writeFile 
