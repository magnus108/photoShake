{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
module PhotoShake.Location
    ( Location(..)
    ) where

import Data.Aeson.TH (deriveJSON, defaultOptions)

data Location 
    = Location FilePath
    | NoLocation
    deriving (Show, Eq)


deriveJSON defaultOptions ''Location
