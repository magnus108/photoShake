{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NoImplicitPrelude #-}                                                                                                                                                                              
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE LambdaCase #-}                                                      

module PhotoShake.Dagsdato
    ( dagsdato
    , yesDagsdato
    , noDagsdato
    , Dagsdato
    ) where

import Data.Eq
import Text.Show
import GHC.Generics
import System.FilePath
import Data.Aeson
import Utils.Actions
import Utils.FP


data Dagsdato 
    = YesDagsdato FilePath
    | NoDagsdato
    deriving (Show, Eq, Generic, ToJSON, FromJSON)


yesDagsdato :: FilePath -> Dagsdato
yesDagsdato = YesDagsdato


noDagsdato :: Dagsdato
noDagsdato = NoDagsdato


dagsdato :: a -> (FilePath -> a) -> Dagsdato -> a
dagsdato f g = \case
    NoDagsdato -> f
    YesDagsdato x -> g x


getDagsdato :: FP -> TerminalM Dagsdato Dagsdato
getDagsdato = readFile 


setDagsdato :: FP -> Dagsdato -> TerminalM Dagsdato ()
setDagsdato = writeFile 
