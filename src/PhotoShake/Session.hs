{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NoImplicitPrelude #-}                                                                                                                                                                              
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE LambdaCase #-}                                                      

module PhotoShake.Session
    ( sessions
    , yesSessions
    , noSessions
    , Session
    , Sessions
    , toInteger
    , session
    , type_
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


data Type 
    = Group
    | Single
    deriving (Show, Eq, Generic, ToJSON, FromJSON)


type_ :: a -> a -> Type -> a
type_ f g = \case
    Single -> f
    Group -> g

data Session
    = Kindergarten Type
    | School
    deriving (Show, Eq, Generic, ToJSON, FromJSON)


session :: (Type -> a) -> a -> Session -> a
session f g = \case
    Kindergarten x -> f x
    School -> g 


toInteger :: Session -> Integer
toInteger = \case
    Kindergarten _ -> 9
    School -> 10


data Sessions 
    = YesSessions (ListZipper Session)
    | NoSessions
    deriving (Show, Eq, Generic, ToJSON, FromJSON)
 

yesSessions :: (ListZipper Session) -> Sessions
yesSessions = YesSessions


noSessions :: Sessions
noSessions = NoSessions


sessions :: a -> (ListZipper Session -> a) -> Sessions -> a
sessions f g = \case
    NoSessions -> f
    YesSessions x -> g x


getSessions :: FP -> TerminalM Sessions Sessions
getSessions = readFile 


setSessions :: FP -> Sessions -> TerminalM Sessions ()
setSessions = writeFile 
