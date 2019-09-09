{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NoImplicitPrelude #-}                                                                                                                                                                              
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE LambdaCase #-}                                                      

module PhotoShake.Dump
    ( dump
    , yesDump
    , noDump
    , Dump
    ) where

import Data.Eq
import Text.Show
import GHC.Generics
import System.FilePath
import Data.Aeson
import System.IO


data Dump 
    = YesDump FilePath
    | NoDump
    deriving (Show, Eq, Generic, ToJSON, FromJSON)


yesDump :: FilePath -> Dump
yesDump = YesDump


noDump :: Dump
noDump = NoDump


dump :: a -> (FilePath -> a) -> Dump -> a
dump f g = \case
    NoDump -> f
    YesDump x -> g x
