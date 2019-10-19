{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NoImplicitPrelude #-}                                                                                                                                                                              
{-# LANGUAGE DeriveAnyClass #-}

module PhotoShake.State
    ( State(..)
    , States(..)
    , getStates
    , setStates
    ) where

import Data.Eq
import Text.Show
import GHC.Generics
    
import System.FilePath


import Data.Aeson

import Utils.ListZipper
import Utils.Actions

import Utils.FP


data State
    = Dump
    | Dagsdato
    | DagsdatoBackup
    | Doneshooting
    | DoneshootingBackup
    | Photographer
    | Camera
    | Shooting
    | Session
    | Location
    | Main
    | Main2
    | Control
    deriving (Show, Eq, Generic, ToJSON, FromJSON)

data States = States (ListZipper State)
    deriving (Show, Eq, Generic, ToJSON, FromJSON)


getStates :: FP -> TerminalM States States
getStates = readFile 


setStates:: FP -> States -> TerminalM States ()
setStates = writeFile 
