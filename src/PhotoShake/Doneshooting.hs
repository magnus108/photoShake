{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NoImplicitPrelude #-}                                                                                                                                                                              
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE LambdaCase #-}                                                      

module PhotoShake.Doneshooting
    ( doneshooting
    , yesDoneshooting
    , noDoneshooting
    , Doneshooting
    , getDoneshooting
    , setDoneshooting
    , toMaybe
    ) where

import Data.Maybe
import Data.Eq
import Text.Show
import GHC.Generics
import System.FilePath
import Data.Aeson
import Utils.Actions
import Utils.FP

import Data.Functor
import Data.Traversable
import Data.Foldable


data DoneshootingF a
    = YesDoneshooting a
    | NoDoneshooting
    deriving (Show, Eq, Generic, ToJSON, FromJSON, Functor, Foldable, Traversable)


type Doneshooting = DoneshootingF FilePath


yesDoneshooting :: FilePath -> Doneshooting
yesDoneshooting = YesDoneshooting


noDoneshooting :: Doneshooting
noDoneshooting = NoDoneshooting


toMaybe :: DoneshootingF a -> Maybe a
toMaybe = doneshooting Nothing Just


doneshooting :: a -> (b -> a) -> DoneshootingF b -> a
doneshooting f g = \case
    NoDoneshooting -> f
    YesDoneshooting x -> g x


getDoneshooting :: FP -> TerminalM Doneshooting Doneshooting
getDoneshooting = readFile 


setDoneshooting :: FP -> Doneshooting -> TerminalM Doneshooting ()
setDoneshooting = writeFile 
