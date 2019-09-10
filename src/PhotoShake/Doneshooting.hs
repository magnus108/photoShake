{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NoImplicitPrelude #-}                                                                                                                                                                              
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE LambdaCase #-}                                                      

module PhotoShake.Doneshooting
    ( doneshooting
    , yesDoneshooting
    , noDoneshooting
    , Doneshooting
    ) where

import Data.Eq
import Text.Show
import GHC.Generics
import System.FilePath
import Data.Aeson
import Utils.Actions
import Utils.FP


data Doneshooting 
    = YesDoneshooting FilePath
    | NoDoneshooting
    deriving (Show, Eq, Generic, ToJSON, FromJSON)


yesDoneshooting :: FilePath -> Doneshooting
yesDoneshooting = YesDoneshooting


noDoneshooting :: Doneshooting
noDoneshooting = NoDoneshooting


doneshooting :: a -> (FilePath -> a) -> Doneshooting -> a
doneshooting f g = \case
    NoDoneshooting -> f
    YesDoneshooting x -> g x


getDoneshooting :: FP -> TerminalM Doneshooting Doneshooting
getDoneshooting = readFile 


setDoneshooting:: FP -> Doneshooting -> TerminalM Doneshooting ()
setDoneshooting = writeFile 
