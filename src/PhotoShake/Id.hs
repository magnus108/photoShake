{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NoImplicitPrelude #-}                                                                                                                                                                              
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE LambdaCase #-}                                                      

module PhotoShake.Id
    ( id
    , yesId
    , noId
    , Id
    , setId
    , getId
    ) where

import Data.String
import Data.Eq
import Text.Show
import GHC.Generics
import System.FilePath
import Data.Aeson
import Utils.Actions
import Utils.FP


data Id 
    = YesId String
    | NoId
    deriving (Show, Eq, Generic, ToJSON, FromJSON)


yesId :: FilePath -> Id
yesId = YesId


noId :: Id
noId = NoId


id :: a -> (FilePath -> a) -> Id -> a
id f g = \case
    NoId -> f
    YesId x -> g x


getId :: FP -> TerminalM Id Id
getId = readFile 


setId :: FP -> Id -> TerminalM Id ()
setId = writeFile 
