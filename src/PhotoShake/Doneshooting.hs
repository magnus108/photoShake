{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
module PhotoShake.Doneshooting
    ( Doneshooting(..)
    ) where

import Data.Aeson.TH (deriveJSON, defaultOptions)

data Doneshooting
    = Doneshooting FilePath
    | NoDoneshooting
    deriving (Show, Eq)


deriveJSON defaultOptions ''Doneshooting
