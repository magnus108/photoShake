{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NoImplicitPrelude #-}                                                                                                                                                                              
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE LambdaCase #-}                                                      

module PhotoShake.Camera
    ( cameras
    , toString
    , yesCameras
    , noCameras
    , getCameras
    , setCameras
    , Camera
    , Cameras
    , jpgCr2
    , jpgCr3
    , camera
    , supported
    ) where


import Data.String
import Data.Eq
import Text.Show
import GHC.Generics
import System.FilePath
import Data.Aeson
import Utils.Actions
import Utils.FP
import Utils.ListZipper

data Camera
    = JpgCr2
    | JpgCr3
    deriving (Show, Eq, Generic, ToJSON, FromJSON)


supported :: [Camera]
supported = [jpgCr2, jpgCr3]

toString :: Camera -> String
toString = camera "cr2" "cr3" 


camera :: a -> a -> Camera -> a
camera f g = \case
    JpgCr2 -> f
    JpgCr3 -> g


jpgCr2 :: Camera
jpgCr2 = JpgCr2

jpgCr3 :: Camera
jpgCr3 = JpgCr3

data Cameras 
    = YesCameras (ListZipper Camera)
    | NoCameras
    deriving (Show, Eq, Generic, ToJSON, FromJSON)
        

yesCameras :: (ListZipper Camera) -> Cameras
yesCameras = YesCameras


noCameras :: Cameras
noCameras = NoCameras


cameras :: a -> (ListZipper Camera -> a) -> Cameras -> a
cameras f g = \case
    NoCameras -> f
    YesCameras x -> g x


getCameras :: FP -> TerminalM Cameras Cameras
getCameras = readFile 


setCameras :: FP -> Cameras -> TerminalM Cameras ()
setCameras = writeFile 
