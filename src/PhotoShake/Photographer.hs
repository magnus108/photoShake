{-# LANGUAGE TemplateHaskell #-}
module PhotoShake.Photographer
    ( Photographer(..)
    , Photographers(..)
    ) where

import Data.Aeson.TH (deriveJSON, defaultOptions)

import Utils.ListZipper


data Photographer = Photographer 
    { name :: String 
    , tid :: String
    }
    deriving (Show, Eq)


deriveJSON defaultOptions ''Photographer

data Photographers = Photographers { unPhotographers :: ListZipper Photographer } deriving (Show)

deriveJSON defaultOptions ''Photographers
