{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NoImplicitPrelude #-}                                                                                                                                                                              
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE LambdaCase #-}                                                      
module PhotoShake.Photographer
    ( photographers
    , yesPhotographers
    , noPhotographers
    , Photographers
    , Photographer
    , _name
    , _tid
    ) where

import Data.Eq
import Data.String
import Text.Show
import GHC.Generics
import System.FilePath
import Data.Aeson
import Utils.Actions
import Utils.FP
import Utils.ListZipper

type Name = String
type Tid = String

data Photographer = Photographer 
    { _name :: Name 
    , _tid :: Tid
    } deriving (Show, Eq, Generic, ToJSON, FromJSON)


data Photographers
    = YesPhotographers (ListZipper Photographer)
    | NoPhotographers 
        deriving (Show, Eq, Generic, ToJSON, FromJSON)


yesPhotographers :: ListZipper Photographer -> Photographers
yesPhotographers = YesPhotographers


noPhotographers :: Photographers
noPhotographers = NoPhotographers


photographers :: a -> (ListZipper Photographer -> a) -> Photographers -> a
photographers f g = \case
    NoPhotographers -> f
    YesPhotographers x -> g x


getPhotographers :: FP -> TerminalM Photographers Photographers
getPhotographers = readFile 


setPhotographers :: FP -> Photographers -> TerminalM Photographers ()
setPhotographers = writeFile 

