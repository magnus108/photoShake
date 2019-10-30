{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DeriveFunctor #-}
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
    , toName
    , toMaybe
    ) where


import Data.Function
import Data.Eq
import Data.Maybe
import Data.Eq
import Data.String
import Text.Show
import GHC.Generics
import System.FilePath
import Data.Aeson
import Utils.Actions
import Utils.FP
import Utils.ListZipper

import Data.Functor
import Data.Traversable
import Data.Foldable

data LocationF a
    = YesLocation a
    | NoLocation
    deriving (Show, Eq, Generic, ToJSON, FromJSON, Functor, Foldable, Traversable)


type Location = LocationF FilePath


yesLocation :: FilePath -> Location
yesLocation = YesLocation


noLocation :: Location
noLocation = NoLocation


location :: a -> (b -> a) -> LocationF b -> a
location f g = \case
    NoLocation -> f
    YesLocation x -> g x


toMaybe :: LocationF a -> Maybe a
toMaybe = location Nothing Just


toName :: Location -> Maybe String
toName = fmap takeBaseName . toMaybe 


getLocation :: FP -> TerminalM Location Location
getLocation = readFile 


setLocation :: FP -> Location -> TerminalM Location ()
setLocation = writeFile 
