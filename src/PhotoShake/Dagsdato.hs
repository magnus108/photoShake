{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
module PhotoShake.Dagsdato
    ( Dagsdato(..)
    ) where

import Data.Aeson.TH (deriveJSON, defaultOptions)

data Dagsdato 
    = Dagsdato { unDagsdato :: FilePath }
    | NoDagsdato
    deriving (Show, Eq)


deriveJSON defaultOptions ''Dagsdato
