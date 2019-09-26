{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
module PhotoShake.Photographee
    (
    ) where


import Data.List (nub)

import Prelude hiding ((++), readFile, filter)
import qualified Prelude as PP ((++))

import GHC.Generics (Generic)
import Data.Csv
import Data.Vector (Vector, find, (++), fromList, toList, filter)
import Data.Char
import qualified Data.ByteString.Lazy as BL

import System.FilePath

import PhotoShake.Photographee2

import PhotoShake.ShakeError
import PhotoShake.Grade
import Control.Exception

import Utils.ListZipper hiding (toList)

import Data.Aeson.TH as DA (deriveJSON, defaultOptions)




