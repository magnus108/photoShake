{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
module PhotoShake.Dump
    ( Dump(..)
    ) where

import Data.Aeson.TH (deriveJSON, defaultOptions)

data Dump = Dump { unDump :: FilePath }
    deriving (Show, Eq)


deriveJSON defaultOptions ''Dump
