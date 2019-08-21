{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
module PhotoShake.Photographer
    ( Photographer(..)
    , Photographers(..)
    ) where

import GHC.Generics
import Data.Aeson

import Utils.ListZipper


data Photographer = Photographer 
    { name :: String 
    , tid :: String
    } deriving (Show, Eq, Generic, ToJSON, FromJSON)


data Photographers
            = Photographers (ListZipper Photographer)
            | NoPhotographers 
                deriving (Show, Eq, Generic, ToJSON, FromJSON)

