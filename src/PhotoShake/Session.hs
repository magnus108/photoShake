{-# LANGUAGE TemplateHaskell #-}
module PhotoShake.Session
    ( Session(..)
    , Sessions(..)
    ) where

import Data.Aeson.TH (deriveJSON, defaultOptions)

import Utils.ListZipper

data Session
    = Kindergarten
    | School
    deriving (Show, Eq)

deriveJSON defaultOptions ''Session

data Sessions = Sessions { unSessions :: ListZipper Session } deriving (Show)

deriveJSON defaultOptions ''Sessions
