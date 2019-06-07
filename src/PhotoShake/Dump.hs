{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
module PhotoShake.Dump
    ( Dump(..)
    ) where

import Data.Aeson.TH (deriveJSON, defaultOptions)

data Dump = Dump FilePath
          | NoDump
                deriving (Show, Eq)


deriveJSON defaultOptions ''Dump
