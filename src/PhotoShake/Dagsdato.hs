{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
module PhotoShake.Dagsdato
    ( Dagsdato(..)
    ) where

import Data.Aeson.TH (deriveJSON, defaultOptions)

data Dagsdato 
    = Dagsdato FilePath
    | NoDagsdato
    deriving (Show, Eq)


deriveJSON defaultOptions ''Dagsdato
