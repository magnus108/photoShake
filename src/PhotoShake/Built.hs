{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
module PhotoShake.Built
    ( Built(..)
    ) where

import PhotoShake.Photographee

import Data.Aeson.TH (deriveJSON, defaultOptions)

data Built 
    = Built Photographee String
    | Building Photographee String
    | NoFind String
    | NoBuilt
    deriving (Show, Eq)


deriveJSON defaultOptions ''Built
