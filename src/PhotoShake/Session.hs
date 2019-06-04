{-# LANGUAGE TemplateHaskell #-}
module PhotoShake.Session
    ( Session(..)
    , Sessions(..)
    , Type(..)
    ) where

import Data.Aeson.TH (deriveJSON, defaultOptions)

import Utils.ListZipper

data Type 
    = Group
    | Single
    deriving (Show, Eq)


deriveJSON defaultOptions ''Type


data Session
    = Kindergarten Type
    | School
    deriving (Show, Eq)


deriveJSON defaultOptions ''Session

data Sessions 
            = Sessions { unSessions :: ListZipper Session }
            | NoSessions
            deriving (Show)

deriveJSON defaultOptions ''Sessions
