{-# LANGUAGE TemplateHaskell #-}
module PhotoShake.Shooting
    ( Shooting(..)
    , Shootings(..)
    ) where

import Data.Aeson.TH (deriveJSON, defaultOptions)

import Utils.ListZipper


data Shooting
    = Omfoto
    | Normal
    deriving (Show)

deriveJSON defaultOptions ''Shooting

data Shootings = Shootings { unShootings :: ListZipper Shooting } deriving (Show)

deriveJSON defaultOptions ''Shootings
