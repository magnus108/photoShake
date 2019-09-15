{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NoImplicitPrelude #-}                                                                                                                                                                              
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE LambdaCase #-}                                                      

module PhotoShake.Shooting
    ( toInteger
    , shootings
    , yesShootings
    , noShootings
    , Shooting
    , Shootings
    , reShoot
    , normal
    ) where


import Prelude (Integer)
import Data.Eq
import Text.Show
import GHC.Generics
import System.FilePath
import Data.Aeson
import Utils.Actions
import Utils.FP
import Utils.ListZipper

data Shooting
    = ReShoot
    | Normal
    deriving (Show, Eq, Generic, ToJSON, FromJSON)


reShoot :: Shooting
reShoot = ReShoot

normal :: Shooting
normal = Normal

toInteger :: Shooting -> Integer
toInteger = \case
    Normal -> 1
    ReShoot -> 2


data Shootings 
    = YesShootings (ListZipper Shooting)
    | NoShootings
    deriving (Show, Eq, Generic, ToJSON, FromJSON)
        

yesShootings :: (ListZipper Shooting) -> Shootings
yesShootings = YesShootings


noShootings :: Shootings
noShootings = NoShootings


shootings :: a -> (ListZipper Shooting -> a) -> Shootings -> a
shootings f g = \case
    NoShootings -> f
    YesShootings x -> g x


getShootings :: FP -> TerminalM Shootings Shootings
getShootings = readFile 


setShootings :: FP -> Shootings -> TerminalM Shootings ()
setShootings = writeFile 
