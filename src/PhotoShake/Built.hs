{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
module PhotoShake.Built
    ( Built(..)
    ) where

import Data.Aeson.TH (deriveJSON, defaultOptions)

data Built 
    = Built String
    | NoBuilt
    deriving (Show, Eq)


deriveJSON defaultOptions ''Built
