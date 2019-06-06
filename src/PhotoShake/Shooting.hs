{-# LANGUAGE TemplateHaskell #-}
module PhotoShake.Shooting
    ( Shooting(..)
    , Shootings(..)
    , encode 
    ) where

import Data.Aeson (encode)
import Data.Aeson.TH (deriveJSON, defaultOptions)

import Utils.ListZipper


data Shooting
    = ReShoot
    | Normal
    deriving (Show, Eq)

deriveJSON defaultOptions ''Shooting

data Shootings 
        = ApprovedShootings (ListZipper Shooting)
        | UnApprovedShootings (ListZipper Shooting)
        | NoShootings
        deriving (Show)
        

deriveJSON defaultOptions ''Shootings
